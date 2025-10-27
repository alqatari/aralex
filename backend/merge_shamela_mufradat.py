#!/usr/bin/env python3
"""
Merge Shamela Mufradat entries into aradicts.db
Adds 383 missing entries from Shamela extraction
"""

import json
import sqlite3
import sys
from pathlib import Path

def merge_shamela_entries():
    """Merge Shamela Mufradat entries into database"""

    # Paths
    db_path = Path(__file__).parent / "aradicts.db"
    shamela_file = Path(__file__).parent.parent / "data" / "arabic_dicts" / "mufradat_entries_shamela.json"

    print(f"📚 Merging Shamela Mufradat entries into database...")
    print(f"   Database: {db_path}")
    print(f"   Shamela file: {shamela_file}")

    # Load Shamela entries
    with open(shamela_file, 'r', encoding='utf-8') as f:
        shamela_entries = json.load(f)

    print(f"   Found {len(shamela_entries)} Shamela entries")

    # Connect to database
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Get Mufradat dict_source_id
    cursor.execute("SELECT id FROM dictionary_sources WHERE dict_id = 'MufradatId'")
    result = cursor.fetchone()
    if not result:
        print("❌ Error: Mufradat dictionary source not found!")
        sys.exit(1)

    mufradat_id = result[0]
    print(f"   Mufradat dict_source_id: {mufradat_id}")

    # Check current entry count
    cursor.execute("SELECT COUNT(*) FROM dictionary_entries WHERE dict_source_id = ?", (mufradat_id,))
    current_count = cursor.fetchone()[0]
    print(f"   Current Mufradat entries: {current_count}")

    # Track statistics
    added = 0
    skipped = 0
    updated = 0

    for entry in shamela_entries:
        root = entry['root']
        root_normalized = entry['root_normalized']
        definition = entry['definition_arabic']

        # Check if this root already exists
        cursor.execute("""
            SELECT id, definition_arabic FROM dictionary_entries
            WHERE dict_source_id = ? AND root_normalized = ?
        """, (mufradat_id, root_normalized))

        existing = cursor.fetchone()

        if existing:
            # Entry exists - check if it's empty or needs update
            existing_id, existing_def = existing
            if not existing_def or existing_def.strip() == "":
                # Update empty entry
                cursor.execute("""
                    UPDATE dictionary_entries
                    SET root = ?, definition_arabic = ?
                    WHERE id = ?
                """, (root, definition, existing_id))
                updated += 1
                print(f"   ✓ Updated: {root_normalized}")
            else:
                # Already has content - skip
                skipped += 1
        else:
            # Insert new entry
            cursor.execute("""
                INSERT INTO dictionary_entries (dict_source_id, root, root_normalized, definition_arabic)
                VALUES (?, ?, ?, ?)
            """, (mufradat_id, root, root_normalized, definition))
            added += 1
            print(f"   ✓ Added: {root_normalized}")

    # Commit changes
    conn.commit()

    # Final count
    cursor.execute("SELECT COUNT(*) FROM dictionary_entries WHERE dict_source_id = ?", (mufradat_id,))
    final_count = cursor.fetchone()[0]

    print(f"\n{'='*60}")
    print(f"Merge Complete!")
    print(f"{'='*60}")
    print(f"Before: {current_count} entries")
    print(f"After:  {final_count} entries")
    print(f"Added:  {added} new entries")
    print(f"Updated: {updated} empty entries")
    print(f"Skipped: {skipped} (already had content)")
    print(f"{'='*60}")

    conn.close()

if __name__ == '__main__':
    merge_shamela_entries()
