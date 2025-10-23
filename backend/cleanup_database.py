#!/usr/bin/env python3
"""
Complete database cleanup:
1. Remove Qamus dictionary entirely
2. Deduplicate remaining 5 dictionaries (Mufradat already done)
3. Vacuum to reclaim space
"""

import sqlite3
import sys
from pathlib import Path

def cleanup_database():
    """Complete database cleanup"""

    db_path = Path(__file__).parent / "aradicts.db"

    print(f"🧹 Complete Database Cleanup")
    print(f"{'='*60}")
    print(f"Database: {db_path}")
    print(f"{'='*60}\n")

    # Connect to database
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    # Get initial stats
    cursor.execute("SELECT COUNT(*) FROM dictionary_entries")
    initial_total = cursor.fetchone()[0]

    cursor.execute("SELECT COUNT(*) FROM dictionary_sources")
    initial_dicts = cursor.fetchone()[0]

    import os
    initial_size_mb = os.path.getsize(db_path) / (1024 * 1024)

    print(f"📊 Initial State:")
    print(f"   Dictionaries: {initial_dicts}")
    print(f"   Total entries: {initial_total:,}")
    print(f"   Database size: {initial_size_mb:.1f} MB\n")

    # STEP 1: Remove Qamus
    print(f"🗑️  Step 1: Removing Qamus dictionary...")

    cursor.execute("SELECT id FROM dictionary_sources WHERE dict_id = 'QamusId'")
    qamus_result = cursor.fetchone()

    if qamus_result:
        qamus_id = qamus_result[0]

        # Count Qamus entries
        cursor.execute("SELECT COUNT(*) FROM dictionary_entries WHERE dict_source_id = ?", (qamus_id,))
        qamus_count = cursor.fetchone()[0]

        # Delete Qamus entries
        cursor.execute("DELETE FROM dictionary_entries WHERE dict_source_id = ?", (qamus_id,))

        # Delete Qamus source
        cursor.execute("DELETE FROM dictionary_sources WHERE id = ?", (qamus_id,))

        print(f"   ✓ Removed Qamus: {qamus_count:,} entries deleted\n")
    else:
        print(f"   ℹ️  Qamus not found (already removed)\n")

    # STEP 2: Deduplicate remaining dictionaries
    print(f"🔧 Step 2: Deduplicating remaining dictionaries...")

    # Get all dictionary IDs except Mufradat (already deduplicated)
    cursor.execute("""
        SELECT id, dict_id, title
        FROM dictionary_sources
        WHERE dict_id != 'MufradatId'
        ORDER BY death_year
    """)

    dicts_to_dedupe = cursor.fetchall()

    total_deleted = 0

    for dict_id, dict_code, dict_title in dicts_to_dedupe:
        # Count before
        cursor.execute("""
            SELECT COUNT(*), COUNT(DISTINCT root_normalized)
            FROM dictionary_entries
            WHERE dict_source_id = ?
        """, (dict_id,))
        before_count, unique_count = cursor.fetchone()
        duplicates = before_count - unique_count

        if duplicates > 0:
            # Delete duplicates (keep highest ID for each root)
            cursor.execute("""
                DELETE FROM dictionary_entries
                WHERE dict_source_id = ?
                AND id NOT IN (
                    SELECT MAX(id)
                    FROM dictionary_entries
                    WHERE dict_source_id = ?
                    GROUP BY root_normalized
                )
            """, (dict_id, dict_id))

            deleted = cursor.rowcount
            total_deleted += deleted

            print(f"   ✓ {dict_title}: removed {deleted:,} duplicates (kept {unique_count:,})")
        else:
            print(f"   ✓ {dict_title}: no duplicates found")

    print(f"\n   Total duplicates removed: {total_deleted:,}\n")

    # Commit changes
    conn.commit()

    # Get final stats before VACUUM
    cursor.execute("SELECT COUNT(*) FROM dictionary_entries")
    after_total = cursor.fetchone()[0]

    cursor.execute("SELECT COUNT(*) FROM dictionary_sources")
    after_dicts = cursor.fetchone()[0]

    print(f"💾 Step 3: Running VACUUM to reclaim space...")
    conn.execute("VACUUM")
    conn.close()

    final_size_mb = os.path.getsize(db_path) / (1024 * 1024)
    space_saved = initial_size_mb - final_size_mb

    print(f"   ✓ Database optimized\n")

    # Final summary
    print(f"{'='*60}")
    print(f"✅ Cleanup Complete!")
    print(f"{'='*60}")
    print(f"\nDictionaries:")
    print(f"  Before: {initial_dicts} dictionaries")
    print(f"  After:  {after_dicts} dictionaries")
    print(f"  Removed: Qamus")

    print(f"\nEntries:")
    print(f"  Before: {initial_total:,} entries")
    print(f"  After:  {after_total:,} entries")
    print(f"  Removed: {initial_total - after_total:,} entries")

    print(f"\nDatabase Size:")
    print(f"  Before: {initial_size_mb:.1f} MB")
    print(f"  After:  {final_size_mb:.1f} MB")
    print(f"  Saved:  {space_saved:.1f} MB")

    print(f"\n{'='*60}")

    # Verify no duplicates remain
    print(f"\n🔍 Verification: Checking for remaining duplicates...")
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT d.title, COUNT(*) as total, COUNT(DISTINCT e.root_normalized) as unique_roots
        FROM dictionary_sources d
        JOIN dictionary_entries e ON d.id = e.dict_source_id
        GROUP BY d.id
        ORDER BY d.death_year
    """)

    all_clean = True
    for title, total, unique in cursor.fetchall():
        if total != unique:
            print(f"   ⚠️  {title}: {total - unique} duplicates remaining")
            all_clean = False
        else:
            print(f"   ✓ {title}: {total:,} entries (no duplicates)")

    if all_clean:
        print(f"\n✅ All dictionaries are clean!")

    conn.close()

if __name__ == '__main__':
    cleanup_database()
