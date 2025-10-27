-- Add normalized text column
ALTER TABLE verses ADD COLUMN text_normalized TEXT;

-- Comprehensive Arabic normalization
UPDATE verses SET text_normalized = 
  -- First remove diacritics
  replace(replace(replace(replace(replace(replace(replace(replace(
    -- Then normalize hamza variations
    replace(replace(replace(replace(replace(
      -- Then normalize alif variations  
      replace(replace(replace(
        -- Finally normalize ya variations
        replace(text,
        -- Diacritics (U+064B to U+0652)
        char(0x064B), ''), char(0x064C), ''), char(0x064D), ''), 
        char(0x064E), ''), char(0x064F), ''), char(0x0650), ''), 
        char(0x0651), ''), char(0x0652), ''),
        -- Hamza forms → plain hamza or remove
        char(0x0623), char(0x0627)), -- أ → ا (alif with hamza above)
        char(0x0625), char(0x0627)), -- إ → ا (alif with hamza below)
        char(0x0622), char(0x0627)), -- آ → ا (alif with madda)
        char(0x0624), char(0x0648)), -- ؤ → و (waw with hamza)
        char(0x0626), char(0x064A)), -- ئ → ي (ya with hamza)
        -- Alif variations
        char(0x0671), char(0x0627)), -- ٱ → ا (alif wasla)
        char(0x0670), ''),            -- ٰ (alif khanjariyah - remove)
        char(0x0649), char(0x064A)),  -- ى → ي (alif maqsura)
        -- Final ya normalization  
        char(0x06CC), char(0x064A));  -- ی → ي (Farsi ya)

-- Create index
CREATE INDEX IF NOT EXISTS idx_text_normalized ON verses(text_normalized);

-- Show sample
SELECT 'Sample normalization:' as info;
SELECT surah, verse, 
  substr(text, 1, 50) as original,
  substr(text_normalized, 1, 50) as normalized  
FROM verses WHERE surah = 12 AND verse BETWEEN 4 AND 6;
