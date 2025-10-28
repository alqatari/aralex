module Main where

import Prelude

import Data.Array (length, null, groupBy)
import Data.Array as Array
import Data.Array.NonEmpty (toArray, head)
import Data.Foldable (traverse_)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Pattern (Pattern(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff (delay, Milliseconds(..))
import Effect.Now (now)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.DateTime.Instant (toDateTime)
import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Decode (decodeJson)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Util.Clipboard (copyToClipboard)
import Util.Arabic (removeTashkeel, filterArabicOnly, hasArabicChars, splitIntoTokens, TextToken(..))
import Util.Time (formatTime)
import Util.RootMatching (wordContainsRoot)
import Util.Grammar as Grammar
import Domain.Dictionary (DictEntry(..), RootText(..))
import Domain.Verse (VerseWithRoot(..), VerseRef(..))
import Domain.Phonosemantics (WordAnalysis(..), LetterBreakdown(..), LetterMeaning(..), LetterCategory(..), LetterAttribute(..), PhonosemanticPattern(..), ArabicLetter(..), DictionaryEvidence(..), QuranicEvidence(..))
import Domain.MorphologyDTO (QuranicWordDTO(..), MorphSegmentDTO(..), MorphFeatureDTO(..))
import Domain.Irab (IrabDetail(..), IrabSource(..))
import Component.EtymologyGraphSimple as EtymologyGraph
import Type.Proxy (Proxy(..))

type State =
  { searchQuery :: String
  , results :: Array DictEntry
  , verses :: Array VerseWithRoot  -- Quranic verses containing the root
  , isSearching :: Boolean
  , error :: Maybe String
  , pageLoadTime :: String
  , expandedDicts :: Map String Boolean  -- Track which dictionaries are expanded
  , expandedVerses :: Map String Boolean  -- Track which verses are expanded
  , expandedSurahs :: Map Int Boolean  -- Track which Surah groups are expanded
  , quranSectionExpanded :: Boolean  -- Track if entire Quran section is expanded
  , dictsSectionExpanded :: Boolean  -- Track if entire Dicts section is expanded
  , hiddenTashkeel :: Map String Boolean  -- Track which boxes have tashkeel hidden
  , copiedItem :: Maybe String  -- Track which item was just copied (for feedback)
  , wordAnalysis :: Map String WordAnalysis  -- Cache phonosemantic analyses by word
  , analyzingWord :: Maybe String  -- Track which word is being analyzed
  , analysisError :: Map String String  -- Track analysis errors by word
  , analysisExpanded :: Boolean  -- Track if analysis panel is expanded
  , legendExpanded :: Boolean  -- Track if color legend is expanded
  , showEtymologyGraph :: Boolean  -- Track if etymology graph modal is shown
  , isRoot :: Boolean  -- Track if current search word is a root word
  , selectedWordForGrammar :: Maybe { verseKey :: String, wordIndex :: Int, word :: QuranicWordDTO }  -- Selected word for grammar panel
  , grammarPanelExpanded :: Boolean  -- Track if grammar panel is expanded
  , hoveredWord :: Maybe { verseKey :: String, wordIndex :: Int }  -- Track hovered word for tooltip
  , verseMorphology :: Map String (Array QuranicWordDTO)  -- Cache morphology by verse key (surah:verse)
  , verseIrab :: Map String (Array IrabDetail)  -- Cache i'rab by verse key (surah:verse)
  }

data Action
  = UpdateSearch String
  | PerformSearch
  | HandleKeyPress KeyboardEvent
  | Initialize String
  | ToggleDict String  -- Toggle dictionary expansion
  | ToggleVerse String  -- Toggle verse expansion
  | ToggleSurah Int  -- Toggle Surah group expansion
  | ToggleQuranSection  -- Toggle entire Quran section
  | ToggleDictsSection  -- Toggle entire Dicts section
  | ExpandAllSurahs  -- Expand all Surah groups
  | CollapseAllSurahs  -- Collapse all Surah groups
  | ExpandAllDicts  -- Expand all dictionary groups
  | CollapseAllDicts  -- Collapse all dictionary groups
  | ToggleTashkeel String  -- Toggle tashkeel visibility for a specific box
  | CopyText String String  -- Copy text to clipboard (key, text)
  | ClearCopyFeedback  -- Clear the copy feedback after timeout
  | AnalyzeWord String  -- Trigger phonosemantic analysis for a word
  | ReceiveAnalysis String WordAnalysis  -- Store successful analysis result
  | AnalysisError String String  -- Handle analysis failure
  | ToggleAnalysis  -- Toggle analysis panel expansion
  | ToggleLegend  -- Toggle color legend expansion
  | ToggleEtymologyGraph  -- Toggle etymology graph modal
  | SelectWordForGrammar String Int QuranicWordDTO  -- Select word for grammar panel (verseKey, wordIndex, word)
  | CloseGrammarPanel  -- Close grammar panel
  | HoverWord (Maybe { verseKey :: String, wordIndex :: Int })  -- Track hovered word
  | FetchMorphology Int Int  -- Fetch morphology for verse (surah, verse)
  | ReceiveMorphology String (Array QuranicWordDTO)  -- Store morphology result (verseKey, words)
  | FetchIrab Int Int  -- Fetch i'rab for verse (surah, verse)
  | ReceiveIrab String (Array IrabDetail)  -- Store i'rab result (verseKey, irabDetails)

-- Slot type for child components
type Slots = (etymologyGraph :: forall q. H.Slot q Void Unit)

_etymologyGraph :: Proxy "etymologyGraph"
_etymologyGraph = Proxy

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just (Initialize "")
        }
    }

initialState :: forall i. i -> State
initialState _ =
  { searchQuery: ""
  , results: []
  , verses: []
  , isSearching: false
  , error: Nothing
  , pageLoadTime: ""
  , expandedDicts: Map.empty
  , expandedVerses: Map.empty
  , expandedSurahs: Map.empty  -- No Surahs expanded initially
  , quranSectionExpanded: true  -- Quran section expanded by default
  , dictsSectionExpanded: true  -- Dicts section expanded by default
  , hiddenTashkeel: Map.empty
  , copiedItem: Nothing
  , wordAnalysis: Map.empty  -- No analyses cached initially
  , analyzingWord: Nothing  -- Not analyzing any word initially
  , analysisError: Map.empty  -- No errors initially
  , analysisExpanded: false  -- Analysis panel collapsed by default
  , legendExpanded: false  -- Color legend collapsed by default
  , showEtymologyGraph: false  -- Graph modal hidden by default
  , isRoot: true  -- Assume root by default
  , selectedWordForGrammar: Nothing  -- No word selected for grammar display
  , grammarPanelExpanded: false  -- Grammar panel collapsed by default
  , hoveredWord: Nothing  -- No word hovered initially
  , verseMorphology: Map.empty  -- No morphology cached initially
  , verseIrab: Map.empty  -- No i'rab cached initially
  }

-- Utility functions imported from Util.* modules

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize _ -> do
    -- Get current time and format it
    instant <- H.liftEffect now
    let dt = toDateTime instant
        timeStr = formatTime dt
    H.modify_ _ { pageLoadTime = timeStr }
    -- Log to console instead of displaying on page
    H.liftEffect $ log $ "Page loaded at: " <> timeStr

  UpdateSearch query -> do
    -- Filter to allow only Arabic characters
    let filtered = filterArabicOnly query
    H.modify_ _ { searchQuery = filtered }

  HandleKeyPress ev -> do
    -- Check if Enter key was pressed
    when (key ev == "Enter") do
      handleAction PerformSearch

  ToggleDict dictName -> do
    -- Toggle the expanded state for this dictionary
    expanded <- H.gets _.expandedDicts
    let newExpanded = Map.alter toggleValue dictName expanded
    H.modify_ _ { expandedDicts = newExpanded }
    where
      toggleValue Nothing = Just true
      toggleValue (Just val) = Just (not val)

  ToggleVerse verseKey -> do
    -- Toggle the expanded state for this verse
    expanded <- H.gets _.expandedVerses
    let newExpanded = Map.alter toggleValue verseKey expanded
    H.modify_ _ { expandedVerses = newExpanded }
    where
      toggleValue Nothing = Just true
      toggleValue (Just val) = Just (not val)

  ToggleSurah surahNum -> do
    -- Toggle the expanded state for this Surah group
    expanded <- H.gets _.expandedSurahs
    let newExpanded = Map.alter toggleValue surahNum expanded
        isNowExpanded = Map.lookup surahNum newExpanded == Just true
    H.modify_ _ { expandedSurahs = newExpanded }

    -- If expanding, fetch morphology for verses in this surah
    when isNowExpanded do
      verses <- H.gets _.verses
      let surahVerses = Array.filter (\(VerseWithRoot v) ->
            let (VerseRef ref) = v.vwrVerseRef
            in ref.refSurah == surahNum
          ) verses
      -- Fetch morphology for each verse in this surah
      traverse_ (\(VerseWithRoot v) ->
        let (VerseRef ref) = v.vwrVerseRef
        in handleAction $ FetchMorphology ref.refSurah ref.refVerse
      ) surahVerses
    where
      toggleValue Nothing = Just true
      toggleValue (Just val) = Just (not val)

  ToggleQuranSection -> do
    -- Toggle the entire Quran section
    H.modify_ \s -> s { quranSectionExpanded = not s.quranSectionExpanded }

  ToggleDictsSection -> do
    -- Toggle the entire Dicts section
    H.modify_ \s -> s { dictsSectionExpanded = not s.dictsSectionExpanded }

  ExpandAllSurahs -> do
    -- Expand all Surah groups
    verses <- H.gets _.verses
    let surahNums = Array.nub $ map (\(VerseWithRoot v) ->
          let (VerseRef ref) = v.vwrVerseRef
          in ref.refSurah
        ) verses
        allExpanded = Map.fromFoldable $ map (\n -> Tuple n true) surahNums
    H.modify_ _ { expandedSurahs = allExpanded }

  CollapseAllSurahs -> do
    -- Collapse all Surah groups
    H.modify_ _ { expandedSurahs = Map.empty }

  ExpandAllDicts -> do
    -- Expand all dictionary groups
    results <- H.gets _.results
    let dictNames = Array.nub $ map (\(DictEntry e) -> e.dictionarySource) results
        allExpanded = Map.fromFoldable $ map (\n -> Tuple n true) dictNames
    H.modify_ _ { expandedDicts = allExpanded }

  CollapseAllDicts -> do
    -- Collapse all dictionary groups
    H.modify_ _ { expandedDicts = Map.empty }

  ToggleTashkeel key -> do
    -- Toggle tashkeel visibility for a specific box
    hidden <- H.gets _.hiddenTashkeel
    let newHidden = Map.alter toggleValue key hidden
    H.modify_ _ { hiddenTashkeel = newHidden }
    where
      toggleValue Nothing = Just true
      toggleValue (Just val) = Just (not val)

  CopyText key text -> do
    -- Copy text to clipboard and show feedback
    liftEffect $ copyToClipboard text
    H.modify_ _ { copiedItem = Just key }
    -- Schedule clearing the feedback after 500ms
    void $ H.fork do
      liftAff $ delay (Milliseconds 500.0)
      handleAction ClearCopyFeedback

  ClearCopyFeedback -> do
    -- Clear the copy feedback
    H.modify_ _ { copiedItem = Nothing }

  AnalyzeWord word -> do
    -- Debug logging
    liftEffect $ log $ "AnalyzeWord action triggered for: " <> word
    -- Check if already analyzing this word or if analysis already exists
    state <- H.get
    case Map.lookup word state.wordAnalysis of
      Just _ -> do
        liftEffect $ log "Analysis already exists, skipping"
        pure unit  -- Already have analysis, do nothing
      Nothing -> do
        liftEffect $ log "Starting analysis..."
        -- Start analyzing
        H.modify_ _ { analyzingWord = Just word, analysisError = Map.delete word state.analysisError }
        -- Make API call
        liftEffect $ log $ "Making API call to: /api/v1/analyze/" <> word
        response <- liftAff $ AX.get ResponseFormat.json
          ("/api/v1/analyze/" <> word)
        case response of
          Left err -> do
            -- Network or HTTP error
            H.modify_ \s -> s
              { analyzingWord = Nothing
              , analysisError = Map.insert word ("Network error: " <> AX.printError err) s.analysisError
              }
          Right result -> do
            -- Try to decode the JSON response
            case decodeJson result.body of
              Left decodeErr -> do
                -- JSON decode error
                H.modify_ \s -> s
                  { analyzingWord = Nothing
                  , analysisError = Map.insert word ("Decode error: " <> show decodeErr) s.analysisError
                  }
              Right analysis -> do
                -- Success! Store the analysis
                handleAction (ReceiveAnalysis word analysis)

  ReceiveAnalysis word analysis -> do
    -- Store successful analysis result and auto-expand panel
    let (WordAnalysis a) = analysis
    liftEffect $ log $ "ReceiveAnalysis: word=" <> word <> ", letterBreakdown count=" <> show (Array.length a.letterBreakdown)
    H.modify_ \s -> s
      { wordAnalysis = Map.insert word analysis s.wordAnalysis
      , analyzingWord = Nothing
      , analysisExpanded = true  -- Auto-expand when analysis completes
      }

  AnalysisError word errorMsg -> do
    -- Handle analysis failure
    H.modify_ \s -> s
      { analyzingWord = Nothing
      , analysisError = Map.insert word errorMsg s.analysisError
      }

  ToggleAnalysis -> do
    -- Toggle the analysis panel expansion
    H.modify_ \s -> s { analysisExpanded = not s.analysisExpanded }

  ToggleLegend -> do
    -- Toggle the color legend expansion
    H.modify_ \s -> s { legendExpanded = not s.legendExpanded }

  ToggleEtymologyGraph -> do
    -- Toggle the etymology graph modal
    H.modify_ \s -> s { showEtymologyGraph = not s.showEtymologyGraph }

  SelectWordForGrammar verseKey wordIndex word -> do
    -- Select a word to display its grammar panel
    H.modify_ \s -> s
      { selectedWordForGrammar = Just { verseKey, wordIndex, word }
      , grammarPanelExpanded = true
      }
    -- Parse surah:verse from verseKey and fetch i'rab
    case String.split (Pattern ":") verseKey of
      [surahStr, verseStr] -> do
        case {surah: floor <$> Number.fromString surahStr, verse: floor <$> Number.fromString verseStr} of
          {surah: Just surah, verse: Just verse} -> do
            handleAction (FetchIrab surah verse)
          _ -> pure unit
      _ -> pure unit

  CloseGrammarPanel -> do
    -- Close the grammar panel
    H.modify_ \s -> s
      { selectedWordForGrammar = Nothing
      , grammarPanelExpanded = false
      }

  HoverWord wordInfo -> do
    -- Track hovered word for tooltip display
    H.modify_ \s -> s { hoveredWord = wordInfo }

  FetchMorphology surah verse -> do
    -- Fetch morphology data for a verse from the API
    let verseKey = show surah <> ":" <> show verse
    -- Check if already cached
    cached <- H.gets (\s -> Map.lookup verseKey s.verseMorphology)
    case cached of
      Just _ -> pure unit  -- Already cached
      Nothing -> do
        -- Fetch from API
        response <- H.liftAff $ AX.get ResponseFormat.json ("/api/v1/morphology/" <> show surah <> "/" <> show verse)
        case response of
          Left err -> do
            -- Ignore errors silently for now
            pure unit
          Right res -> do
            case decodeJson res.body of
              Left _ -> pure unit
              Right (words :: Array QuranicWordDTO) -> do
                -- Store fetched morphology in cache directly
                H.modify_ \s -> s { verseMorphology = Map.insert verseKey words s.verseMorphology }

  ReceiveMorphology verseKey words -> do
    -- Store fetched morphology in cache
    H.modify_ \s -> s { verseMorphology = Map.insert verseKey words s.verseMorphology }

  FetchIrab surah verse -> do
    -- Fetch i'rab data for a verse from the API
    let verseKey = show surah <> ":" <> show verse
    -- Check if already cached
    cached <- H.gets (\s -> Map.lookup verseKey s.verseIrab)
    case cached of
      Just _ -> pure unit  -- Already cached
      Nothing -> do
        -- Fetch from API
        response <- H.liftAff $ AX.get ResponseFormat.json ("/api/v1/irab/" <> show surah <> "/" <> show verse)
        case response of
          Left err -> do
            -- Ignore errors silently for now
            pure unit
          Right res -> do
            case decodeJson res.body of
              Left _ -> pure unit
              Right (irabDetails :: Array IrabDetail) -> do
                -- Store fetched i'rab in cache directly
                H.modify_ \s -> s { verseIrab = Map.insert verseKey irabDetails s.verseIrab }

  ReceiveIrab verseKey irabDetails -> do
    -- Store fetched i'rab in cache
    H.modify_ \s -> s { verseIrab = Map.insert verseKey irabDetails s.verseIrab }

  PerformSearch -> do
    query <- H.gets _.searchQuery
    -- Only search if we have Arabic text
    when (query /= "" && hasArabicChars query) do
      H.modify_ _
        { isSearching = true
        , error = Nothing
        , results = []
        , verses = []
        -- Collapse all sections except analysis panel on new search
        , quranSectionExpanded = false
        , dictsSectionExpanded = false
        , analysisExpanded = true  -- Keep analysis panel expanded
        , legendExpanded = false
        , showEtymologyGraph = false
        }

      -- Make API calls in parallel (verses and dictionary)
      versesResponse <- H.liftAff $ AX.get ResponseFormat.json
        ("/api/v1/verses/" <> query)

      dictResponse <- H.liftAff $ AX.get ResponseFormat.json
        ("/api/v1/dictionary/" <> query)

      -- Process verses response
      case versesResponse of
        Left _ -> do
          H.modify_ _ { isSearching = false, error = Just "Failed to fetch verses" }

        Right vRes -> do
          case decodeJson vRes.body of
            Left _ -> do
              H.modify_ _ { isSearching = false, error = Just "Invalid verses response from server" }

            Right (verses :: Array VerseWithRoot) -> do
              -- Process dictionary response
              case dictResponse of
                Left _ -> do
                  H.modify_ _ { isSearching = false, error = Just "Failed to fetch dictionary entries", verses = verses }

                Right dRes -> do
                  case decodeJson dRes.body of
                    Left _ -> do
                      H.modify_ _ { isSearching = false, error = Just "Invalid dictionary response from server", verses = verses }

                    Right (entries :: Array DictEntry) -> do
                      -- Detect if this is a root word (has dictionary entries)
                      let isRootWord = not (null entries)
                      H.modify_ _ { isSearching = false, results = entries, verses = verses, isRoot = isRootWord }
                      -- Automatically trigger phonosemantic analysis for the search query
                      handleAction (AnalyzeWord query)

-- Helper: Highlight words containing the root (preserves Arabic text shaping)
-- Get phonosemantic color for a single Arabic letter
getLetterPhonosemanticColor :: String -> String
getLetterPhonosemanticColor letter = case letter of
  -- Emphatics (orange)
  "ط" -> "#f97316"
  "ض" -> "#f97316"
  "ص" -> "#f97316"
  "ظ" -> "#f97316"
  -- Gutturals (purple)
  "ء" -> "#a855f7"
  "ه" -> "#a855f7"
  "ع" -> "#a855f7"
  "ح" -> "#a855f7"
  "غ" -> "#a855f7"
  "خ" -> "#a855f7"
  -- Labials (blue)
  "ب" -> "#3b82f6"
  "م" -> "#3b82f6"
  "و" -> "#3b82f6"
  "ف" -> "#3b82f6"
  -- Default (purple)
  _ -> "#667eea"

-- Render a word with root letters highlighted (preserves Arabic cursive joining)
renderWordWithLetterColors :: forall m. String -> Array Char -> H.ComponentHTML Action Slots m
renderWordWithLetterColors word rootLetters =
  -- NOTE: We cannot color individual letters because wrapping each letter in HTML <span>
  -- tags breaks Arabic contextual letter forms (cursive joining). Arabic letters change
  -- shape based on position (initial, medial, final, isolated), and separating them with
  -- HTML tags forces them into isolated forms, breaking the natural cursive flow.
  -- Therefore, we use a single bold red style for the entire word containing root letters.
  HH.span
    [ HP.style "font-weight: 700; color: #e74c3c;" ]
    [ HH.text word ]

highlightRootInText :: forall m. String -> String -> H.ComponentHTML Action Slots m
highlightRootInText root text =
  -- Split into tokens (Arabic words and non-Arabic text) to preserve formatting
  let rootLetters = toCharArray (removeTashkeel root)
      tokens = splitIntoTokens text
      renderToken token = case token of
        ArabicWord word ->
          if wordContainsRoot word root
            then renderWordWithLetterColors (removeTashkeel word) rootLetters
            else HH.text word
        NonArabic nonArabic ->
          HH.text nonArabic
  in HH.span_ (map renderToken tokens)

-- Render Quranic verses containing the root, grouped by Surah
renderVerses :: forall m. State -> Array VerseWithRoot -> H.ComponentHTML Action Slots m
renderVerses state verses =
  let
    -- Group verses by Surah number
    surahGroups = groupBySurah verses
    -- Calculate total occurrences across all verses
    totalOccurrences = Array.foldl (\acc (VerseWithRoot v) -> acc + v.vwrOccurrences) 0 verses
  in
    HH.div
      [ HP.style "margin-bottom: 24px; max-width: 800px; margin-left: auto; margin-right: auto;" ]
      [ -- Collapsible header for entire Quran section
        HH.div
          [ HP.style "padding: 16px 20px; background: linear-gradient(135deg, #10b981 0%, #059669 100%); color: white; border-radius: 8px; display: flex; justify-content: space-between; align-items: center; user-select: none; margin-bottom: 16px;"
          ]
          [ -- Left side: Collapse/expand button and title (whole area clickable)
            HH.div
              [ HP.style "display: flex; align-items: center; gap: 12px; flex: 1; cursor: pointer;"
              , HE.onClick \_ -> ToggleQuranSection
              ]
              [ -- Expand/collapse button
                HH.button
                  [ HP.type_ HP.ButtonButton
                  , HP.style "background: rgba(255,255,255,0.2); color: white; border: none; padding: 4px 12px; border-radius: 6px; font-size: 1.2rem;"
                  ]
                  [ HH.text $ if state.quranSectionExpanded then "▼" else "◀" ]
              , HH.h3
                  [ HP.style "margin: 0; font-family: 'RB Regular', sans-serif; font-size: 1.4rem; display: flex; align-items: center; gap: 10px;" ]
                  [ HH.text "الآيات القرآنية"
                  -- Show indicator if word is not a root
                  , if not state.isRoot
                      then HH.span
                        [ HP.style "background: rgba(255,255,255,0.3); padding: 4px 12px; border-radius: 12px; font-size: 0.9rem; font-weight: bold;" ]
                        [ HH.text "[ليست جذراً]" ]
                      else HH.text ""
                  ]
              ]
          -- Right side: Count badge (showing occurrences, verses, and surahs)
          , HH.span
              [ HP.style "background: rgba(255,255,255,0.2); padding: 6px 16px; border-radius: 16px; font-weight: bold;" ]
              [ HH.text $ show totalOccurrences <> " مرة في " <> show (length verses) <> " آية في " <> show (Array.length surahGroups) <> " سورة" ]
          ]
      -- Surah groups (only shown when section is expanded)
      , if state.quranSectionExpanded
          then HH.div
            [ HP.class_ (HH.ClassName "surah-groups") ]
            (map (renderSurahGroup state) surahGroups)
          else HH.div_ []
      ]
  where
    -- Group verses by Surah number
    groupBySurah :: Array VerseWithRoot -> Array (Tuple Int (Array VerseWithRoot))
    groupBySurah vs =
      let
        groups = groupBy (\(VerseWithRoot a) (VerseWithRoot b) ->
          let (VerseRef refA) = a.vwrVerseRef
              (VerseRef refB) = b.vwrVerseRef
          in refA.refSurah == refB.refSurah
        ) vs
      in map (\g ->
        let VerseWithRoot first = head g
            (VerseRef ref) = first.vwrVerseRef
            arr = toArray g
        in Tuple ref.refSurah arr
      ) groups

    -- Render a Surah group with collapsible content
    renderSurahGroup :: State -> Tuple Int (Array VerseWithRoot) -> H.ComponentHTML Action Slots m
    renderSurahGroup st (Tuple surahNum surahVerses) =
      let
        isExpanded = Map.lookup surahNum st.expandedSurahs == Just true
        -- Get Surah name from first verse
        VerseWithRoot firstVerse = case Array.head surahVerses of
          Just v -> v
          Nothing -> VerseWithRoot { vwrVerseRef: VerseRef { refSurah: 0, refVerse: 0 }, vwrText: "", vwrRoot: "", vwrSurahName: "", vwrOccurrences: 0, vwrWordIndices: [] }
        surahName = firstVerse.vwrSurahName
        verseCount = Array.length surahVerses
        -- Calculate total occurrences in this Surah
        surahOccurrences = Array.foldl (\acc (VerseWithRoot v) -> acc + v.vwrOccurrences) 0 surahVerses
      in HH.div
        [ HP.style "margin-bottom: 16px; border: 2px solid #e5e7eb; border-radius: 8px; overflow: hidden; background: white;" ]
        [ -- Surah header (clickable to expand/collapse)
          HH.div
            [ HP.style $ "padding: 12px 16px; background: #f3f4f6; color: #374151; display: flex; justify-content: space-between; align-items: center; user-select: none; cursor: pointer;"
            , HE.onClick \_ -> ToggleSurah surahNum
            ]
            [ HH.div
                [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                [ -- Expand/collapse button
                  HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.style "background: #e5e7eb; color: #6b7280; border: none; padding: 4px 12px; border-radius: 6px; font-size: 1.2rem;"
                    ]
                    [ HH.text $ if isExpanded then "▼" else "◀" ]
                , HH.span
                    [ HP.style "font-family: 'RB Regular', sans-serif; font-size: 1.2rem;" ]
                    [ HH.text $ "سورة " <> surahName ]
                ]
            , HH.span
                [ HP.style "background: #e5e7eb; color: #374151; padding: 4px 12px; border-radius: 12px; font-size: 0.9rem; font-weight: bold;" ]
                [ HH.text $ show surahOccurrences <> " مرة في " <> show verseCount <> " آية" ]
            ]
        -- Verses within this Surah (only shown when expanded)
        , if isExpanded
            then HH.div
              [ HP.style "padding: 8px;" ]
              (map (renderVerse st) surahVerses)
            else HH.div_ []
        ]

    -- Render verse as clickable words with morphology
    renderVerseWords :: State -> String -> String -> Int -> Int -> Array QuranicWordDTO -> H.ComponentHTML Action Slots m
    renderVerseWords st verseKey searchRoot surah verse words =
      HH.div
        [ HP.class_ (HH.ClassName "verse-text")
        , HP.style "direction: rtl; font-family: 'Scheherazade New', serif; font-size: 1.8rem; line-height: 2.2; cursor: pointer; display: flex; flex-wrap: wrap; gap: 4px;"
        ]
        (Array.mapWithIndex (\idx (QuranicWordDTO word) ->
          let isSelected = case st.selectedWordForGrammar of
                Just sel -> sel.verseKey == verseKey && sel.wordIndex == idx
                Nothing -> false
              isHovered = case st.hoveredWord of
                Just hov -> hov.verseKey == verseKey && hov.wordIndex == idx
                Nothing -> false
              wordColor = Grammar.posColor word.dtoWordPOS
              -- Check if this word's root matches the searched root (from database, not pattern matching!)
              wordMatchesRoot = case word.dtoWordRoot of
                Just root -> removeTashkeel root == removeTashkeel searchRoot
                Nothing -> false
              -- Base styles for all words
              baseStyle = "padding: 2px 4px; margin: 0 2px; border-radius: 4px; transition: all 0.2s;"
              -- Root highlighting: bold red color for matching words
              rootStyle = if wordMatchesRoot then "font-weight: 700; color: #e74c3c;" else ""
              -- Selection highlighting
              selectedStyle = if isSelected then "background: " <> wordColor <> "20; border-bottom: 2px solid " <> wordColor <> ";" else ""
              -- Hover highlighting
              hoverStyle = if isHovered then "background: " <> wordColor <> "10;" else ""
          in HH.span
            [ HP.style $ baseStyle <> rootStyle <> selectedStyle <> hoverStyle
            , HE.onClick \_ -> SelectWordForGrammar verseKey idx (QuranicWordDTO word)
            , HE.onMouseEnter \_ -> HoverWord (Just { verseKey, wordIndex: idx })
            , HE.onMouseLeave \_ -> HoverWord Nothing
            , HP.title $ Grammar.posToArabic word.dtoWordPOS  -- Quick preview on hover
            ]
            [ HH.text word.dtoFullSurface ]
        ) words)

    -- Render individual verse within a Surah group
    renderVerse :: State -> VerseWithRoot -> H.ComponentHTML Action Slots m
    renderVerse st (VerseWithRoot verse) =
      let (VerseRef ref) = verse.vwrVerseRef
          verseKey = show ref.refSurah <> ":" <> show ref.refVerse
          tashkeelHidden = Map.lookup ("verse-" <> verseKey) st.hiddenTashkeel == Just true
          wasCopied = st.copiedItem == Just ("verse-" <> verseKey)
          displayText = if tashkeelHidden then removeTashkeel verse.vwrText else verse.vwrText
          -- Check if we have morphology for this verse
          morphology = Map.lookup verseKey st.verseMorphology
      in HH.div
        [ HP.style "margin-bottom: 12px; padding: 12px; border: 1px solid #e5e7eb; border-radius: 6px; background: white;" ]
        [ -- Header container with verse ref and buttons
          HH.div
            [ HP.style "display: flex; align-items: center; justify-content: space-between; gap: 8px; margin-bottom: 8px;" ]
            [ -- Verse reference (left side) - not clickable anymore
              HH.div
                [ HP.style "display: flex; align-items: center; gap: 8px; flex: 1;" ]
                [ HH.text $ "آية " <> show ref.refVerse
                , if verse.vwrOccurrences > 1
                    then HH.span
                      [ HP.style "margin-right: 8px; background: #fef3c7; color: #92400e; padding: 2px 8px; border-radius: 8px; font-size: 0.8rem; font-weight: bold;" ]
                      [ HH.text $ "×" <> show verse.vwrOccurrences ]
                    else HH.text ""
                ]
            -- Buttons (right side)
            , HH.div
                [ HP.style "display: flex; gap: 8px;" ]
                [ -- Copy button
                  HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.style "padding: 6px 10px; font-size: 1.2rem; background: #667eea; color: white; border: none; border-radius: 4px; cursor: pointer; display: flex; align-items: center; justify-content: center; min-width: 36px;"
                    , HP.title "نسخ النص"
                    , HE.onClick \_ -> CopyText ("verse-" <> verseKey) verse.vwrText
                    ]
                    [ HH.i
                        [ HP.class_ (HH.ClassName "material-icons")
                        , HP.style "font-size: 18px;"
                        ]
                        [ HH.text $ if wasCopied then "check" else "content_copy" ]
                    ]
                -- Tashkeel toggle button
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.style "padding: 6px 10px; font-size: 1.2rem; background: #764ba2; color: white; border: none; border-radius: 4px; cursor: pointer; display: flex; align-items: center; justify-content: center; min-width: 36px;"
                    , HP.title "إخفاء/إظهار التشكيل"
                    , HE.onClick \_ -> ToggleTashkeel ("verse-" <> verseKey)
                    ]
                    [ HH.i
                        [ HP.class_ (HH.ClassName "material-icons")
                        , HP.style "font-size: 18px;"
                        ]
                        [ HH.text "format_size" ]
                    ]
                ]
            ]
        -- Verse text (clickable words if morphology available, otherwise plain text)
        , case morphology of
            Just words ->
              -- Render as clickable words with morphology (with root highlighting)
              renderVerseWords st verseKey verse.vwrRoot ref.refSurah ref.refVerse words
            Nothing ->
              -- No morphology yet - render plain text and trigger fetch
              HH.div
                [ HP.class_ (HH.ClassName "verse-text")
                , HP.ref (H.RefLabel verseKey)  -- Reference to trigger fetch on mount
                ]
                [ highlightRootInText verse.vwrRoot displayText ]
        -- Grammar panel (if word is selected from this verse)
        , case st.selectedWordForGrammar of
            Just sel | sel.verseKey == verseKey ->
              renderGrammarPanel st sel.word
            _ -> HH.div_ []
        ]

-- Phonosemantic Analysis Rendering Functions

-- Main analysis panel
-- Render the collapsible analysis panel (shows after search box, before results)
renderAnalysisPanel :: forall m. State -> H.ComponentHTML Action Slots m
renderAnalysisPanel state =
  -- Show analysis for the current search query (not cached old results)
  if state.searchQuery == ""
    then HH.div
      [ HP.style "padding: 20px; text-align: center; color: #999; font-size: 0.9rem;" ]
      [ HH.text "ابحث عن جذر لرؤية التحليل الصوتي الدلالي هنا" ]
    else
      let
        -- Get analysis for the CURRENT search query
        currentAnalysis = Map.lookup state.searchQuery state.wordAnalysis
      in
        case currentAnalysis of
          Nothing -> HH.div_ []  -- No analysis yet for current search
          Just (WordAnalysis analysis) ->
            HH.div
              [ HP.style "margin: 0 auto 20px auto; max-width: 800px; border-radius: 12px; box-shadow: 0 4px 12px rgba(248, 113, 113, 0.2); overflow: hidden;" ]
              [ -- Header with composite meaning and buttons
                HH.div
                  [ HP.style "background: linear-gradient(135deg, #f87171 0%, #dc2626 100%); padding: 16px 20px; display: flex; justify-content: space-between; align-items: center;"
                  ]
                  [ -- Left: Collapse/expand button
                    HH.button
                      [ HP.type_ HP.ButtonButton
                      , HP.style "background: rgba(255,255,255,0.2); color: white; border: none; padding: 4px 12px; border-radius: 6px; cursor: pointer; font-size: 1.2rem; margin-left: 12px;"
                      , HE.onClick \_ -> ToggleAnalysis
                      ]
                      [ HH.text $ if state.analysisExpanded then "▼" else "◀" ]
                  -- Middle: Composite meaning (clickable to collapse/expand)
                  , HH.h3
                      [ HP.style "margin: 0; color: white; font-family: 'RB Regular', sans-serif; font-size: 1.4rem; direction: rtl; flex: 1; cursor: pointer; user-select: none;"
                      , HE.onClick \_ -> ToggleAnalysis
                      ]
                      [ HH.text analysis.compositeMeaning ]
                  -- Right: Buttons
                  , HH.div
                      [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                      [ -- Copy button
                        HH.button
                          [ HP.type_ HP.ButtonButton
                          , HP.style "padding: 6px 12px; background: rgba(255,255,255,0.2); color: white; border: none; border-radius: 6px; cursor: pointer; display: flex; align-items: center; gap: 4px; font-size: 0.9rem;"
                          , HE.onClick \_ -> CopyText "composite-meaning" analysis.compositeMeaning
                          ]
                          [ HH.span [ HP.class_ (HH.ClassName "material-icons"), HP.style "font-size: 18px;" ] [ HH.text "content_copy" ]
                          , HH.text $ case state.copiedItem of
                              Just key | key == "composite-meaning" -> "نُسخ"
                              _ -> "نسخ"
                          ]
                      ]
                  ]


              -- Expandable details (letter breakdown + patterns) - NO padding to avoid nested box feel
              , if state.analysisExpanded
                  then HH.div_
                    [ renderLetterBreakdown state analysis.letterBreakdown
                    , if Array.null analysis.patterns
                        then HH.div_ []
                        else renderPatterns analysis.patterns
                    ]
                  else HH.div_ []
              ]

renderWordAnalysis :: forall m. State -> WordAnalysis -> H.ComponentHTML Action Slots m
renderWordAnalysis state (WordAnalysis analysis) =
  HH.div
    [ HP.style "margin-top: 16px; padding: 16px; background: linear-gradient(135deg, #f5f7fa 0%, #e8eef5 100%); border-radius: 8px; border: 2px solid #667eea;" ]
    [ -- Header
      HH.div
        [ HP.style "margin-bottom: 12px; padding-bottom: 8px; border-bottom: 2px solid #667eea;" ]
        [ HH.h4
            [ HP.style "margin: 0; color: #667eea; font-family: 'Effra Regular', sans-serif; font-size: 1.3rem;" ]
            [ HH.text "التحليل الصوتي الدلالي" ]
        ]
    -- Composite meaning
    , renderCompositeMeaning analysis.compositeMeaning
    -- Letter breakdown table
    , renderLetterBreakdown state analysis.letterBreakdown
    -- Patterns (if any)
    , if Array.null analysis.patterns
        then HH.text ""
        else renderPatterns analysis.patterns
    ]

-- Composite meaning display
renderCompositeMeaning :: forall m. String -> H.ComponentHTML Action Slots m
renderCompositeMeaning compositeMeaning =
  HH.div
    [ HP.style "margin-bottom: 16px; padding: 12px; background: white; border-radius: 6px; border-left: 4px solid #764ba2;" ]
    [ HH.h5
        [ HP.style "margin: 0 0 8px 0; color: #764ba2; font-family: 'Effra Regular', sans-serif; font-size: 1.1rem;" ]
        [ HH.text "المعنى المركب" ]
    , HH.p
        [ HP.style "margin: 0; font-family: 'Effra Regular', sans-serif; font-size: 1.1rem; line-height: 1.8; color: #333; direction: rtl;" ]
        [ HH.text compositeMeaning ]
    ]

-- Letter breakdown table
renderLetterBreakdown :: forall m. State -> Array LetterBreakdown -> H.ComponentHTML Action Slots m
renderLetterBreakdown state letters =
  HH.div_
    [ -- Table (no title, no margin)
      HH.table
        [ HP.style "width: 100%; border-collapse: collapse; background: white;" ]
        [ HH.thead_
            [ HH.tr
                [ HP.style "background: linear-gradient(135deg, #f87171 0%, #dc2626 100%); color: white;" ]
                [ HH.th [ HP.style "padding: 12px; text-align: center; font-family: 'Effra Regular', sans-serif; font-size: 1.3rem; width: 15%;" ] [ HH.text "الحرف" ]
                , HH.th [ HP.style "padding: 12px; text-align: center; font-family: 'Effra Regular', sans-serif; font-size: 1.2rem; width: 25%;" ] [ HH.text "التصنيف" ]
                , HH.th [ HP.style "padding: 12px 20px; text-align: right; font-family: 'Effra Regular', sans-serif; font-size: 1.2rem; direction: rtl; width: 60%;" ] [ HH.text "المعنى" ]
                ]
            ]
        , HH.tbody_
            (map renderLetterRow letters)
        ]
    -- Color legend AFTER the table (clickable to expand/collapse)
    , HH.div
        [ HP.style "margin: 0; background: #f3f4f6; border-top: 1px solid #e5e7eb;" ]
        [ -- Legend header (clickable)
          HH.div
            [ HP.style "padding: 12px 16px; display: flex; align-items: center; gap: 12px; cursor: pointer; user-select: none; flex-wrap: wrap;"
            , HE.onClick \_ -> ToggleLegend
            ]
            ([ -- Triangle button
              HH.button
                [ HP.type_ HP.ButtonButton
                , HP.style "background: #e5e7eb; color: #6b7280; border: none; padding: 4px 12px; border-radius: 6px; font-size: 1.2rem;"
                ]
                [ HH.text $ if state.legendExpanded then "▼" else "◀" ]
            , HH.span
                [ HP.style "font-family: 'Effra Regular', sans-serif; font-size: 1.1rem; font-weight: 600; color: #374151;" ]
                [ HH.text "دليل الألوان" ]
            ] <> if not state.legendExpanded
                  then -- Compact legend items when collapsed
                    [ HH.span [ HP.style "display: flex; align-items: center; gap: 4px; margin-right: 8px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #dc2626; border-radius: 2px;" ] []
                        , HH.text "حنجرية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #7c3aed; border-radius: 2px;" ] []
                        , HH.text "حلقية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #db2777; border-radius: 2px;" ] []
                        , HH.text "لهوية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #0891b2; border-radius: 2px;" ] []
                        , HH.text "طبقية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #d97706; border-radius: 2px;" ] []
                        , HH.text "حنكية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #059669; border-radius: 2px;" ] []
                        , HH.text "لثوية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #92400e; border-radius: 2px;" ] []
                        , HH.text "أسنانية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #2563eb; border-radius: 2px;" ] []
                        , HH.text "شفوية"
                        ]
                    , HH.span [ HP.style "display: flex; align-items: center; gap: 4px; font-family: 'Effra Regular', sans-serif; font-size: 0.9rem;" ]
                        [ HH.span [ HP.style "width: 14px; height: 14px; background: #f97316; border-radius: 2px;" ] []
                        , HH.text "إطباقية"
                        ]
                    ]
                  else []
            )
        -- Legend content (shown only when expanded) - 3 columns: color, name+letters, definition
        , if state.legendExpanded
            then HH.div
              [ HP.style "padding: 12px 16px; display: flex; flex-direction: column; gap: 8px; font-size: 0.9rem; direction: rtl; font-family: 'Effra Regular', sans-serif;" ]
              [ -- Glottal (back of throat)
                HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #dc2626; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "حنجرية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ء، هـ، غ، خ" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من الحنجرة (الحلق الأسفل)" ]
                  ]
              -- Pharyngeal
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #7c3aed; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "حلقية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ع، ح" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من الحلق (الوسط والأعلى)" ]
                  ]
              -- Uvular
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #db2777; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "لهوية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ق" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الصوت الذي يخرج من اللهاة وأقصى اللسان" ]
                  ]
              -- Velar
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #0891b2; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "طبقية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ك" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الصوت الذي يخرج من الطبق (أقصى الحنك اللين)" ]
                  ]
              -- Palatal
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #d97706; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "حنكية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ج، ش، ي" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من وسط الحنك واللسان" ]
                  ]
              -- Alveolar
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #059669; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "لثوية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ت، د، ن، ل، ر، ز، س" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من اللثة وطرف اللسان" ]
                  ]
              -- Dental
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #92400e; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "أسنانية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ث، ذ، ظ" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من الأسنان واللسان" ]
                  ]
              -- Labial (front)
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #2563eb; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "شفوية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ب، م، و، ف" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات التي تخرج من الشفتين أو بمساعدتهما" ]
                  ]
              -- Emphatic (special feature)
              , HH.div [ HP.style "display: flex; align-items: center; gap: 12px;" ]
                  [ HH.span [ HP.style "width: 20px; height: 20px; background: #f97316; border-radius: 3px; flex-shrink: 0;" ] []
                  , HH.span [ HP.style "font-weight: 600; min-width: 90px;" ] [ HH.text "إطباقية:" ]
                  , HH.span [ HP.style "font-size: 1.2rem; min-width: 150px;" ] [ HH.text "ط، ض، ص، ظ" ]
                  , HH.span [ HP.style "color: #6b7280; font-size: 0.95rem; font-family: 'Effra Regular', sans-serif;" ] [ HH.text "الأصوات المفخمة التي يرتفع فيها أقصى اللسان نحو الحنك" ]
                  ]
              ]
            else HH.div_ []
        ]
    ]
  where
    -- Helper to check if letter has emphatic attribute
    hasEmphatic :: Array LetterAttribute -> Boolean
    hasEmphatic attrs = Array.any isEmphatic attrs
      where
        isEmphatic Emphatic = true
        isEmphatic _ = false

    -- Get color for letter based on phonetic attributes (8 categories)
    getLetterColor :: Array LetterAttribute -> LetterCategory -> String
    getLetterColor attrs category =
      if hasEmphatic attrs
        then "#f97316"  -- Orange for emphatics (ط، ض، ص، ظ) - HIGHEST PRIORITY
        else case category of
          Labial -> "#2563eb"      -- Strong Blue for labials (ب، م، و، ف)
          Dental -> "#92400e"      -- Brown for dentals (ث، ذ)
          Alveolar -> "#059669"    -- Dark Green for alveolars (ت، د، ن، ل، ر، ز، س)
          Palatal -> "#d97706"     -- Dark Amber for palatals (ج، ش، ي)
          Velar -> "#0891b2"       -- Teal for velars (ك)
          Uvular -> "#db2777"      -- Strong Pink for uvulars (ق)
          Pharyngeal -> "#7c3aed"  -- Vivid Purple for pharyngeals (ع، ح)
          Glottal -> "#dc2626"     -- Red for glottals (ء، ه)

    renderLetterRow :: LetterBreakdown -> H.ComponentHTML Action Slots m
    renderLetterRow (LetterBreakdown lb) =
      let bgColor = if lb.position `mod` 2 == 0 then "#fff0f0" else "#fffafa"  -- Light reddish alternating rows
          (ArabicLetter letterRec) = lb.letterChar
          letterColor = getLetterColor lb.attributes lb.category
      in HH.tr
        [ HP.style ("background: " <> bgColor <> "; border-bottom: 1px solid #e5e7eb;") ]
        [ HH.td [ HP.style ("padding: 12px; text-align: center; font-family: 'Effra Regular', sans-serif; font-size: 2rem; font-weight: bold; color: " <> letterColor <> " !important;") ] [ HH.text letterRec.unLetter ]
        , HH.td [ HP.style ("padding: 12px; text-align: center; font-family: 'Effra Regular', sans-serif; font-size: 1.3rem; font-weight: 600; color: " <> letterColor <> ";") ] [ HH.text $ showCategory lb.category ]
        , HH.td [ HP.style "padding: 12px 20px; text-align: right; font-family: 'Effra Regular', sans-serif; font-size: 1.2rem; direction: rtl; color: #333; line-height: 1.8;" ]
            [ case lb.meaning of
                Just (LetterMeaning m) -> HH.text m.primaryMeaning
                Nothing -> HH.span [ HP.style "color: #9ca3af; font-style: italic;" ] [ HH.text "لا يوجد معنى" ]
            ]
        ]

    showCategory :: LetterCategory -> String
    showCategory cat = case cat of
      Labial -> "شفوي"
      Dental -> "أسناني"
      Alveolar -> "لثوي"
      Palatal -> "حنكي"
      Velar -> "طبقي"
      Uvular -> "لهوي"
      Pharyngeal -> "حلقي"
      Glottal -> "حنجري"

-- Pattern badges
renderPatterns :: forall m. Array PhonosemanticPattern -> H.ComponentHTML Action Slots m
renderPatterns patterns =
  HH.div
    [ HP.style "margin-bottom: 16px;" ]
    [ HH.h5
        [ HP.style "margin: 0 0 8px 0; color: #667eea; font-family: 'Effra Regular', sans-serif; font-size: 1.1rem;" ]
        [ HH.text "الأنماط الصوتية المكتشفة" ]
    , HH.div
        [ HP.style "display: flex; flex-wrap: wrap; gap: 8px;" ]
        (map renderPattern patterns)
    ]
  where
    renderPattern :: PhonosemanticPattern -> H.ComponentHTML Action Slots m
    renderPattern (PhonosemanticPattern p) =
      HH.div
        [ HP.style "padding: 8px 12px; background: white; border: 2px solid #667eea; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);" ]
        [ HH.div
            [ HP.style "font-family: 'Effra Regular', sans-serif; color: #667eea; font-size: 1rem; margin-bottom: 4px;" ]
            [ HH.text $ showPatternName p.patternName ]
        , HH.div
            [ HP.style "font-size: 0.85rem; color: #6b7280; margin-bottom: 4px;" ]
            [ HH.text $ "الحروف: " <> String.joinWith "، " (map (\(ArabicLetter l) -> l.unLetter) p.letterSequence) ]
        , HH.div
            [ HP.style "font-size: 0.95rem; color: #333; direction: rtl; font-family: 'Effra Regular', sans-serif;" ]
            [ HH.text $ translateSemanticEffect p.semanticEffect ]
        ]

    showPatternName :: String -> String
    showPatternName name = case name of
      "intensification" -> "التفخيم"
      "guttural_depth" -> "العمق الحلقي"
      "labial_containment" -> "الاحتواء الشفوي"
      "movement" -> "الحركة"
      "opening" -> "الانفتاح"
      _ -> name

    translateSemanticEffect :: String -> String
    translateSemanticEffect effect =
      if String.contains (Pattern "emphatic") effect
        then "الحروف الإطباقية تضيف شدة وقوة إلى معنى الكلمة"
      else if String.contains (Pattern "guttural") effect
        then "تجمع الأصوات الحلقية يوحي بالعمق والجلال أو المعنى الروحي"
      else if String.contains (Pattern "labial") effect
        then "الحروف الشفوية توحي بمعاني الاحتواء والإحاطة أو الجمع"
      else if String.contains (Pattern "vibration") effect || String.contains (Pattern "movement") effect
        then "حرف الراء يوحي بالحركة والاهتزاز أو التكرار بسبب نطقه المرتجف"
      else if String.contains (Pattern "opening") effect || String.contains (Pattern "revealing") effect
        then "حرف الفاء يوحي بالانفتاح والكشف أو الانفصال"
      else effect

-- Dictionary evidence section
renderDictionaryEvidence :: forall m. Array DictionaryEvidence -> H.ComponentHTML Action Slots m
renderDictionaryEvidence evidence =
  if Array.null evidence
    then HH.div_ []
    else HH.div
      [ HP.style "margin-top: 24px;" ]
      [ HH.h5
          [ HP.style "margin: 0 0 8px 0; color: #667eea; font-family: 'Effra Regular', sans-serif; font-size: 1.1rem;" ]
          [ HH.text $ "دليل معجمي (" <> show (Array.length evidence) <> " معجم)" ]
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
          (map renderDictEvidence evidence)
      ]
  where
    renderDictEvidence :: DictionaryEvidence -> H.ComponentHTML Action Slots m
    renderDictEvidence (DictionaryEvidence ev) =
      let (RootText rootStr) = ev.dictRoot
      in HH.div
        [ HP.style "padding: 12px; background: white; border-right: 4px solid #667eea; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);" ]
        [ HH.div
            [ HP.style "font-family: 'Effra Regular', sans-serif; color: #667eea; font-size: 1rem; margin-bottom: 6px; direction: rtl;" ]
            [ HH.text ev.dictName ]
        , HH.div
            [ HP.style "font-family: 'Effra Regular', sans-serif; font-size: 0.95rem; color: #333; direction: rtl; line-height: 1.6;" ]
            [ HH.text ev.relevantExcerpt ]
        ]

-- Quranic evidence section
renderQuranicEvidence :: forall m. Array QuranicEvidence -> H.ComponentHTML Action Slots m
renderQuranicEvidence evidence =
  if Array.null evidence
    then HH.div_ []
    else HH.div
      [ HP.style "margin-top: 24px;" ]
      [ HH.h5
          [ HP.style "margin: 0 0 8px 0; color: #667eea; font-family: 'Effra Regular', sans-serif; font-size: 1.1rem;" ]
          [ HH.text $ "دليل قرآني (" <> show (Array.length evidence) <> " آية)" ]
      , HH.div
          [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
          (map renderQuranicEv evidence)
      ]
  where
    renderQuranicEv :: QuranicEvidence -> H.ComponentHTML Action Slots m
    renderQuranicEv (QuranicEvidence ev) =
      let (VerseRef ref) = ev.quranicVerseRef
      in HH.div
        [ HP.style "padding: 12px; background: white; border-right: 4px solid #764ba2; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);" ]
        [ HH.div
            [ HP.style "font-family: 'Effra Regular', sans-serif; color: #764ba2; font-size: 1rem; margin-bottom: 6px; direction: rtl;" ]
            [ HH.text $ "سورة " <> show ref.refSurah <> " آية " <> show ref.refVerse ]
        , HH.div
            [ HP.style "font-family: 'Effra Regular', sans-serif; font-size: 1.1rem; color: #333; direction: rtl; line-height: 1.8;" ]
            [ HH.text ev.wordContext ]
        ]

-- Render message for non-root words
renderNonRootMessage :: forall m. State -> H.ComponentHTML Action Slots m
renderNonRootMessage state =
  HH.div
    [ HP.style "margin-bottom: 24px; max-width: 800px; margin-left: auto; margin-right: auto;" ]
    [ HH.div
        [ HP.style "padding: 16px 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 8px; text-align: center; font-family: 'RB Regular', sans-serif; font-size: 1.4rem;" ]
        [ HH.h3
            [ HP.style "margin: 0 0 12px 0;" ]
            [ HH.text "المعاجم العربية" ]
        , HH.div
            [ HP.style "background: rgba(255,255,255,0.2); padding: 12px 20px; border-radius: 8px; font-size: 1.1rem; margin-top: 12px;" ]
            [ HH.text "المعاجم تعرض محتوى الجذور فقط. الكلمة المبحوث عنها ليست جذراً." ]
        , renderExternalLinksMessage
        ]
    ]

-- Render external dictionary links message (shown always)
renderExternalLinksMessage :: forall m. H.ComponentHTML Action Slots m
renderExternalLinksMessage =
  HH.div
    [ HP.style "background: rgba(255,255,255,0.15); padding: 12px 20px; border-radius: 8px; font-size: 1rem; margin-top: 12px; text-align: center; direction: rtl;" ]
    [ HH.text "للبحث الشامل، استخدم: "
    , HH.a
        [ HP.href "https://dohadictionary.org"
        , HP.target "_blank"
        , HP.style "color: #fbbf24; text-decoration: underline; font-weight: bold; margin: 0 6px;"
        ]
        [ HH.text "قاموس الدوحة" ]
    , HH.text " أو "
    , HH.a
        [ HP.href "https://tafsir.app"
        , HP.target "_blank"
        , HP.style "color: #fbbf24; text-decoration: underline; font-weight: bold; margin: 0 6px;"
        ]
        [ HH.text "تطبيق التفسير" ]
    ]

-- Render dictionary results grouped by dictionary
renderResults :: forall m. State -> Array DictEntry -> H.ComponentHTML Action Slots m
renderResults state entries =
  let grouped = groupByDict entries
  in HH.div
    [ HP.style "margin-bottom: 24px; max-width: 800px; margin-left: auto; margin-right: auto;" ]
    [ -- Collapsible header for entire Dicts section
      HH.div
        [ HP.style "padding: 16px 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border-radius: 8px; display: flex; justify-content: space-between; align-items: center; user-select: none; margin-bottom: 16px;"
        ]
        [ -- Left side: Collapse/expand button and title (whole area clickable)
          HH.div
            [ HP.style "display: flex; align-items: center; gap: 12px; flex: 1; cursor: pointer;"
            , HE.onClick \_ -> ToggleDictsSection
            ]
            [ -- Expand/collapse button
              HH.button
                [ HP.type_ HP.ButtonButton
                , HP.style "background: rgba(255,255,255,0.2); color: white; border: none; padding: 4px 12px; border-radius: 6px; font-size: 1.2rem;"
                ]
                [ HH.text $ if state.dictsSectionExpanded then "▼" else "◀" ]
            , HH.h3
                [ HP.style "margin: 0; font-family: 'RB Regular', sans-serif; font-size: 1.4rem;" ]
                [ HH.text "المعاجم العربية" ]
            ]
        -- Right side: Count badge
        , HH.span
            [ HP.style "background: rgba(255,255,255,0.2); padding: 6px 16px; border-radius: 16px; font-weight: bold;" ]
            [ HH.text $ show (length entries) <> " مدخل في " <> show (length grouped) <> " معجم" ]
        ]
    -- External links message (shown always when section is expanded)
    , if state.dictsSectionExpanded
        then renderExternalLinksMessage
        else HH.div_ []
    -- Dictionary groups (only shown when section is expanded)
    , if state.dictsSectionExpanded
        then HH.div
          [ HP.class_ (HH.ClassName "dict-groups") ]
          (map (renderDictGroup state) grouped)
        else HH.div_ []
    ]
  where
    -- Group entries by dictionary source (preserves backend's chronological order)
    groupByDict :: Array DictEntry -> Array (Tuple String (Array DictEntry))
    groupByDict es =
      -- Don't sort - backend already returns in chronological order (oldest to newest)
      let groups = groupBy (\(DictEntry a) (DictEntry b) -> a.dictionarySource == b.dictionarySource) es
      in map (\g ->
              let DictEntry first = head g
                  arr = toArray g
              in Tuple first.dictionarySource arr
            ) groups

    -- Render a dictionary group with collapsible content
    renderDictGroup :: State -> Tuple String (Array DictEntry) -> H.ComponentHTML Action Slots m
    renderDictGroup st (Tuple dictName entries') =
      let isExpanded = Map.lookup dictName st.expandedDicts == Just true
      in HH.div
        [ HP.class_ (HH.ClassName "dict-group") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "dict-header")
            , HE.onClick \_ -> ToggleDict dictName
            ]
            [ HH.span
                [ HP.class_ (HH.ClassName "expand-icon") ]
                [ HH.text $ if isExpanded then "▼ " else "◀ " ]
            , HH.h4
                [ HP.class_ (HH.ClassName "dict-name")
                , HP.style "font-family: 'Effra Regular', sans-serif;"
                ]
                [ HH.text dictName ]
            , HH.span
                [ HP.class_ (HH.ClassName "entry-count") ]
                [ HH.text $ "(" <> show (length entries') <> " مدخل)" ]
            ]
        , if isExpanded
            then HH.div
              [ HP.class_ (HH.ClassName "dict-entries") ]
              (map renderEntry entries')
            else HH.div_ []
        ]

    renderEntry :: DictEntry -> H.ComponentHTML Action Slots m
    renderEntry (DictEntry entry) =
      -- Since there's only one entry per dictionary for a root, we can use just the dictionary source as key
      let entryKey = "dict-" <> entry.dictionarySource
          -- Unwrap RootText to get the String - use normalized root for consistent matching
          (RootText rootRec) = entry.rootNormalized
          rootStr = rootRec.unRoot
          tashkeelHidden = Map.lookup entryKey state.hiddenTashkeel == Just true
          wasCopied = state.copiedItem == Just entryKey
          displayText = if tashkeelHidden then removeTashkeel entry.definitionArabic else entry.definitionArabic
      in HH.div
        [ HP.class_ (HH.ClassName "dictionary-entry") ]
        [ -- Header with buttons
          HH.div
            [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 12px;" ]
            [ -- Copy button
              HH.button
                [ HP.type_ HP.ButtonButton
                , HP.style "padding: 6px 10px; font-size: 1.2rem; background: #667eea; color: white; border: none; border-radius: 4px; cursor: pointer; display: flex; align-items: center; justify-content: center; min-width: 36px;"
                , HP.title "نسخ النص"
                , HE.onClick \_ -> CopyText entryKey entry.definitionArabic
                ]
                [ HH.i
                    [ HP.class_ (HH.ClassName "material-icons")
                    , HP.style "font-size: 18px;"
                    ]
                    [ HH.text $ if wasCopied then "check" else "content_copy" ]
                ]
            -- Tashkeel toggle button
            , HH.button
                [ HP.type_ HP.ButtonButton
                , HP.style "padding: 6px 10px; font-size: 1.2rem; background: #764ba2; color: white; border: none; border-radius: 4px; cursor: pointer; display: flex; align-items: center; justify-content: center; min-width: 36px;"
                , HP.title "إخفاء/إظهار التشكيل"
                , HE.onClick \_ -> ToggleTashkeel entryKey
                ]
                [ HH.i
                    [ HP.class_ (HH.ClassName "material-icons")
                    , HP.style "font-size: 18px;"
                    ]
                    [ HH.text "format_size" ]
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "entry-definition") ]
            [ highlightRootInText rootStr displayText ]
        ]

-- Etymology Graph Panel (Collapsible Inline)
renderEtymologyPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderEtymologyPanel state =
  -- Only show etymology panel if we have an analysis with a root for the current search query
  let
    currentAnalysis = Map.lookup state.searchQuery state.wordAnalysis
    maybeRoot = currentAnalysis >>= (\(WordAnalysis analysis) -> analysis.analysisRoot)
  in
    case maybeRoot of
      Nothing -> HH.div_ []  -- No analysis or no root yet, don't show panel
      Just (RootText rootRec) ->
        HH.div
          [ HP.style "margin: 0 auto 20px auto; max-width: 800px; border-radius: 12px; box-shadow: 0 4px 12px rgba(102, 126, 234, 0.2); overflow: hidden;" ]
          [ -- Header with toggle
            HH.div
              [ HP.style "background: linear-gradient(135deg, #4a90e2 0%, #667eea 100%); padding: 16px 20px; display: flex; justify-content: space-between; align-items: center; cursor: pointer;"
              , HE.onClick \_ -> ToggleEtymologyGraph
              ]
              [ -- Left: Collapse/expand button
                HH.button
                  [ HP.type_ HP.ButtonButton
                  , HP.style "background: rgba(255,255,255,0.2); color: white; border: none; padding: 4px 12px; border-radius: 6px; cursor: pointer; font-size: 1.2rem; margin-left: 12px;"
                  , HE.onClick \_ -> ToggleEtymologyGraph
                  ]
                  [ HH.text $ if state.showEtymologyGraph then "▼" else "◀" ]
              -- Middle: Title (clickable to collapse/expand)
              , HH.h3
                  [ HP.style "margin: 0; color: white; font-family: 'RB Regular', sans-serif; font-size: 1.4rem; direction: rtl; flex: 1; display: flex; align-items: center; gap: 8px;"
                  ]
                  [ HH.span [ HP.class_ (HH.ClassName "material-icons"), HP.style "font-size: 24px;" ] [ HH.text "account_tree" ]
                  , HH.text "شبكة العلاقات الاشتقاقية "
                  , HH.span [ HP.style "font-size: 0.9rem; opacity: 0.8;" ] [ HH.text "(Experimental)" ]
                  ]
              ]
          -- Expandable Graph Content
          , if state.showEtymologyGraph
              then HH.div
                  [ HP.style "background: linear-gradient(135deg, #f9fafb 0%, #f3f4f6 100%); padding: 20px; min-height: 600px;" ]
                  [ HH.slot _etymologyGraph unit EtymologyGraph.component rootRec.unRoot absurd ]
              else HH.div_ []
          ]

-- Grammar Panel (إعراب) - Arabic only
renderGrammarPanel :: forall m. State -> QuranicWordDTO -> H.ComponentHTML Action Slots m
renderGrammarPanel state (QuranicWordDTO word) =
  HH.div
    [ HP.class_ (HH.ClassName "grammar-panel")
    , HP.style "border-radius: 12px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); overflow: hidden; background: white; margin-top: 16px;"
    ]
    [ -- Header
      HH.div
        [ HP.style $ "background: " <> Grammar.posColor word.dtoWordPOS <> "; padding: 16px 20px; display: flex; justify-content: space-between; align-items: center;" ]
        [ HH.h3
            [ HP.style "margin: 0; color: white; font-family: 'Scheherazade New', serif; font-size: 1.6rem; direction: rtl;" ]
            [ HH.text $ "إعراب: " <> word.dtoFullSurface ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.style "background: rgba(255,255,255,0.2); color: white; border: none; padding: 8px 12px; border-radius: 6px; cursor: pointer;"
            , HP.title "إغلاق"
            , HE.onClick \_ -> CloseGrammarPanel
            ]
            [ HH.i [ HP.class_ (HH.ClassName "material-icons") ] [ HH.text "close" ] ]
        ]
    -- Content (scrollable)
    , HH.div
        [ HP.style "padding: 20px; direction: rtl; font-family: 'Readex Pro', sans-serif; max-height: 600px; overflow-y: auto;" ]
        [ -- Basic info grid
          HH.div
            [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 12px; margin-bottom: 16px;" ]
            [ renderInfoBox "النوع" (Grammar.posToArabic word.dtoWordPOS) (Grammar.posColor word.dtoWordPOS)
            , case word.dtoWordRoot of
                Just root -> renderInfoBox "الجذر" root "#6366F1"
                Nothing -> HH.div_ []
            , case word.dtoWordLemma of
                Just lemma -> renderInfoBox "اللمة" lemma "#8B5CF6"
                Nothing -> HH.div_ []
            ]
        -- Detailed features (segment-by-segment breakdown) - horizontal grid layout
        , HH.div
            [ HP.style "border-top: 1px solid #e5e7eb; padding-top: 16px;" ]
            [ HH.h4
                [ HP.style "margin: 0 0 12px 0; font-size: 1.1rem; color: #374151;" ]
                [ HH.text "تحليل المقاطع:" ]
            , HH.div
                [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px;" ]
                (map renderSegmentWithFeatures word.dtoSegments)
            ]
        -- I'rab section (traditional grammatical analysis)
        , case state.selectedWordForGrammar of
            Just sel -> renderIrabSection state sel.verseKey (sel.wordIndex + 1)
            Nothing -> HH.div_ []
        ]
    ]
  where
    renderInfoBox :: String -> String -> String -> H.ComponentHTML Action Slots m
    renderInfoBox label value color =
      HH.div
        [ HP.style $ "background: " <> color <> "15; border-right: 3px solid " <> color <> "; padding: 12px; border-radius: 6px;" ]
        [ HH.div
            [ HP.style "font-size: 0.85rem; color: #6b7280; margin-bottom: 4px;" ]
            [ HH.text label ]
        , HH.div
            [ HP.style "font-size: 1.1rem; font-weight: 600; color: #111827;" ]
            [ HH.text value ]
        ]

    renderSegmentWithFeatures :: MorphSegmentDTO -> H.ComponentHTML Action Slots m
    renderSegmentWithFeatures (MorphSegmentDTO seg) =
      let
        segmentType = inferSegmentType seg.dtoFeatures
        segmentColor = case segmentType of
          "سابقة" -> "#3b82f6"  -- Blue for prefix
          "لاحقة" -> "#8b5cf6"  -- Purple for suffix
          _ -> "#10b981"        -- Green for stem
      in
        HH.div
          [ HP.style $ "background: " <> segmentColor <> "15; border-right: 3px solid " <> segmentColor <> "; padding: 12px; border-radius: 6px;" ]
          [ -- Segment type label (small, on top)
            HH.div
              [ HP.style "font-size: 0.85rem; color: #6b7280; margin-bottom: 4px;" ]
              [ HH.text segmentType ]
          -- Segment surface form (large, prominent)
          , HH.div
              [ HP.style "font-family: 'Scheherazade New', serif; font-size: 1.3rem; font-weight: 600; color: #111827; margin-bottom: 8px;" ]
              [ HH.text seg.dtoSurface ]
          -- Features as badges (compact)
          , HH.div
              [ HP.style "display: flex; flex-wrap: wrap; gap: 4px;" ]
              (seg.dtoFeatures >>= renderFeatureBadge)
          ]

    inferSegmentType :: Array MorphFeatureDTO -> String
    inferSegmentType features =
      let
        hasPrefix = Array.any (\f -> case f of
          PrefixDTO -> true
          _ -> false
        ) features
        hasSuffix = Array.any (\f -> case f of
          SuffixDTO -> true
          _ -> false
        ) features
      in
        if hasPrefix then "سابقة"
        else if hasSuffix then "لاحقة"
        else "جذع"

    renderFeatureBadge :: MorphFeatureDTO -> Array (H.ComponentHTML Action Slots m)
    renderFeatureBadge feat =
      case Grammar.featureToShortArabic feat of
        Just label ->
          [ HH.span
              [ HP.style "background: #e0e7ff; color: #3730a3; padding: 6px 12px; border-radius: 16px; font-size: 0.9rem; white-space: nowrap; font-weight: 500;" ]
              [ HH.text label ]
          ]
        Nothing -> []

-- Render i'rab (traditional grammatical analysis) section
renderIrabSection :: forall m. State -> String -> Int -> H.ComponentHTML Action Slots m
renderIrabSection state verseKey wordPosition =
  case Map.lookup verseKey state.verseIrab of
    Nothing ->
      -- No i'rab data yet - could be loading
      HH.div
        [ HP.style "border-top: 1px solid #e5e7eb; padding-top: 16px; margin-top: 16px;" ]
        [ HH.div
            [ HP.style "text-align: center; padding: 20px; color: #9ca3af;" ]
            [ HH.text "جاري تحميل الإعراب..." ]
        ]
    Just irabList ->
      case Array.find (\(IrabDetail irab) -> irab.irabWordPosition == wordPosition) irabList of
        Nothing ->
          -- No i'rab for this specific word
          HH.div
            [ HP.style "border-top: 1px solid #e5e7eb; padding-top: 16px; margin-top: 16px;" ]
            [ HH.div
                [ HP.style "text-align: center; padding: 20px; color: #9ca3af;" ]
                [ HH.text "لا يوجد إعراب متاح لهذه الكلمة" ]
            ]
        Just (IrabDetail irab) ->
          HH.div
            [ HP.style "border-top: 1px solid #e5e7eb; padding-top: 16px; margin-top: 16px;" ]
            [ HH.h4
                [ HP.style "margin: 0 0 12px 0; font-size: 1.1rem; color: #374151;" ]
                [ HH.text "الإعراب التقليدي:" ]
            -- Full i'rab text (main display)
            , HH.div
                [ HP.style "background: #fef3c7; border-right: 4px solid #f59e0b; padding: 16px; border-radius: 8px; margin-bottom: 12px;" ]
                [ HH.div
                    [ HP.style "font-family: 'Scheherazade New', serif; font-size: 1.3rem; line-height: 1.8; color: #78350f;" ]
                    [ HH.text irab.irabFullText ]
                ]
            -- Detailed breakdown grid
            , HH.div
                [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 12px;" ]
                [ case irab.irabGrammaticalRole of
                    Just role -> renderIrabBox "الدور النحوي" role "#10b981"
                    Nothing -> HH.div_ []
                , case irab.irabConstructionType of
                    Just construction -> renderIrabBox "نوع التركيب" construction "#3b82f6"
                    Nothing -> HH.div_ []
                , renderIrabBox "نوع الإعراب" irab.irabCaseType "#8b5cf6"
                , case irab.irabCaseMarker of
                    Just marker -> renderIrabBox "علامة الإعراب" marker "#ef4444"
                    Nothing -> HH.div_ []
                , case irab.irabCasePosition of
                    Just position -> renderIrabBox "المحل الإعرابي" position "#f59e0b"
                    Nothing -> HH.div_ []
                ]
            -- Source and confidence indicator
            , HH.div
                [ HP.style "margin-top: 12px; display: flex; justify-content: space-between; align-items: center; font-size: 0.85rem; color: #6b7280;" ]
                [ HH.div_
                    [ HH.text $ "المصدر: " <> case irab.irabSource of
                        Inferred -> "مستنتج من المدونة الصرفية للقرآن الكريم"
                        AljadwalOCR -> "الجدول في إعراب القرآن"
                        Manual -> "تحقيق يدوي"
                    ]
                , HH.div_
                    [ HH.text $ "الثقة: " <> show (floor (irab.irabConfidence * 100.0)) <> "%" ]
                ]
            ]
  where
    renderIrabBox :: String -> String -> String -> H.ComponentHTML Action Slots m
    renderIrabBox label value color =
      HH.div
        [ HP.style $ "background: " <> color <> "15; border-right: 3px solid " <> color <> "; padding: 12px; border-radius: 6px;" ]
        [ HH.div
            [ HP.style "font-size: 0.85rem; color: #6b7280; margin-bottom: 4px;" ]
            [ HH.text label ]
        , HH.div
            [ HP.style "font-size: 1.1rem; font-weight: 600; color: #111827; font-family: 'Scheherazade New', serif;" ]
            [ HH.text value ]
        ]

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "app-container") ]
    [ -- Header
      HH.header
        [ HP.class_ (HH.ClassName "header") ]
        [ HH.h1
            [ HP.class_ (HH.ClassName "app-title") ]
            [ HH.text "تحليل الدلالات الصوتية للحروف" ]
        , HH.p
            [ HP.class_ (HH.ClassName "subtitle") ]
            [ HH.text "بلسان عربي مبين" ]
        ]

    -- Search Section
    , HH.div
        [ HP.class_ (HH.ClassName "search-section") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "search-form") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "search-input-group") ]
                [ HH.input
                    [ HP.type_ HP.InputText
                    , HP.class_ (HH.ClassName "search-input")
                    , HP.placeholder "ابحث عن جذر... (مثال: كتب)"
                    , HP.value state.searchQuery
                    , HE.onValueInput UpdateSearch
                    , HE.onKeyDown HandleKeyPress
                    , HP.attr (HH.AttrName "lang") "ar"
                    , HP.attr (HH.AttrName "dir") "rtl"
                    , HP.attr (HH.AttrName "inputmode") "text"
                    , HP.autofocus true
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonButton
                    , HP.class_ (HH.ClassName "search-button")
                    , HP.disabled state.isSearching
                    , HE.onClick \_ -> PerformSearch
                    ]
                    [ HH.text "بحث" ]
                ]
            ]
        ]

    -- Results section
    , HH.div
        [ HP.class_ (HH.ClassName "results-section") ]
        [ -- Show error if any
          case state.error of
            Just errMsg ->
              HH.div
                [ HP.class_ (HH.ClassName "error-message") ]
                [ HH.text errMsg ]
            Nothing -> HH.div_ []

        -- Phonosemantic Analysis Panel (collapsible, appears after search)
        , renderAnalysisPanel state

        -- Etymology Graph Panel (collapsible, inline)
        , renderEtymologyPanel state

        -- Show results
        , if state.isSearching
            then HH.div
              [ HP.class_ (HH.ClassName "loading") ]
              [ HH.text "Loading..." ]
            else HH.div_
              [ -- Show verses FIRST (priority)
                if not (null state.verses)
                  then renderVerses state state.verses
                  else if state.searchQuery /= "" && not state.isSearching
                    then HH.div_ []  -- Show nothing if no verses found
                    else HH.div_ []

              -- Show dictionary results SECOND (or message if not a root)
              , if not (null state.results)
                  then renderResults state state.results
                  else if state.searchQuery /= "" && not state.isSearching && not state.isRoot
                    then renderNonRootMessage state
                    else HH.div_ []
              ]
        ]

    -- Info Footer
    , HH.footer
        [ HP.style "text-align: center; padding: 20px; background: #f3f4f6; color: #374151; font-family: 'Cascadia Code', monospace; font-size: 1rem; direction: ltr;" ]
        [ HH.a
            [ HP.href "https://wa.me/966566574248"
            , HP.target "_blank"
            , HP.style "color: #10b981; text-decoration: none; font-weight: bold;"
            ]
            [ HH.text "+966_5665_7_4248" ]
        ]

    ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
