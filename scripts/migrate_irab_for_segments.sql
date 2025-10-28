-- Migration: Add segment support to irab_details table
-- This allows storing multiple i'rab entries per word (one per segment)

-- 1. Create new table with segment_position field
CREATE TABLE irab_details_new (
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Location (now includes segment)
  surah INTEGER NOT NULL,
  verse INTEGER NOT NULL,
  word_position INTEGER NOT NULL,
  segment_position INTEGER NOT NULL DEFAULT 1,  -- NEW: which segment (1-indexed)

  -- Segment identification
  segment_surface TEXT NOT NULL,  -- Changed from word_surface

  -- Parsed irab components
  grammatical_role TEXT,
  construction_type TEXT,
  case_type TEXT,
  case_marker TEXT,
  case_position TEXT,

  -- Full text
  full_irab_text TEXT NOT NULL,

  -- Metadata
  source TEXT NOT NULL,
  confidence REAL DEFAULT 0.8,
  volume_number INTEGER,
  page_number INTEGER,

  -- Timestamps
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  -- Constraints (now includes segment_position)
  UNIQUE(surah, verse, word_position, segment_position, source)
);

-- 2. Copy existing data (treating all as segment 1)
INSERT INTO irab_details_new (
  surah, verse, word_position, segment_position, segment_surface,
  grammatical_role, construction_type, case_type, case_marker, case_position,
  full_irab_text, source, confidence, volume_number, page_number,
  created_at, updated_at
)
SELECT
  surah, verse, word_position, 1, word_surface,
  grammatical_role, construction_type, case_type, case_marker, case_position,
  full_irab_text, source, confidence, volume_number, page_number,
  created_at, updated_at
FROM irab_details;

-- 3. Drop old table and rename new one
DROP TABLE irab_details;
ALTER TABLE irab_details_new RENAME TO irab_details;

-- 4. Recreate indexes
CREATE INDEX idx_irab_verse ON irab_details(surah, verse);
CREATE INDEX idx_irab_word ON irab_details(surah, verse, word_position);
CREATE INDEX idx_irab_role ON irab_details(grammatical_role);
CREATE INDEX idx_irab_source ON irab_details(source);
