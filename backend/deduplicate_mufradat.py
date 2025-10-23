#!/usr/bin/env python3
"""
Remove duplicate Mufradat entries from database
Keep entries with higher IDs (newer) and remove older duplicates
"""

import sqlite3
import sys
from pathlib import Path

def deduplicate_mufradat():
    """Remove duplicate Mufradat entries"""

    db_path = Path(__file__).parent / "aradicts.db"

    print(f"🧹 Deduplicating Mufradat entries in database...")
    print(f"   Database: {db_path}")

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

    # Count before
    cursor.execute("SELECT COUNT(*), COUNT(DISTINCT root_normalized) FROM dictionary_entries WHERE dict_source_id = ?",
                   (mufradat_id,))
    total_before, unique_before = cursor.fetchone()
    print(f"\n   Before: {total_before} entries, {unique_before} unique roots")
    print(f"   Duplicates: {total_before - unique_before}")

    # Find and delete duplicates (keep the one with highest ID for each root_normalized)
    print(f"\n   Removing duplicates...")

    cursor.execute("""
        DELETE FROM dictionary_entries
        WHERE dict_source_id = ?
        AND id NOT IN (
            SELECT MAX(id)
            FROM dictionary_entries
            WHERE dict_source_id = ?
            GROUP BY root_normalized
        )
    """, (mufradat_id, mufradat_id))

    deleted_count = cursor.rowcount
    print(f"   ✓ Deleted {deleted_count} duplicate entries")

    # Commit changes
    conn.commit()

    # Count after
    cursor.execute("SELECT COUNT(*), COUNT(DISTINCT root_normalized) FROM dictionary_entries WHERE dict_source_id = ?",
                   (mufradat_id,))
    total_after, unique_after = cursor.fetchone()

    # Check total database size
    cursor.execute("SELECT COUNT(*) FROM dictionary_entries")
    db_total = cursor.fetchone()[0]

    # Get database file size
    import os
    db_size_mb = os.path.getsize(db_path) / (1024 * 1024)

    print(f"\n{'='*60}")
    print(f"Deduplication Complete!")
    print(f"{'='*60}")
    print(f"Mufradat entries:")
    print(f"  Before: {total_before}")
    print(f"  After:  {total_after}")
    print(f"  Deleted: {deleted_count}")
    print(f"\nDatabase stats:")
    print(f"  Total entries: {db_total}")
    print(f"  Database size: {db_size_mb:.1f} MB")
    print(f"{'='*60}")

    # Verify no duplicates remain
    cursor.execute("""
        SELECT root_normalized, COUNT(*) as count
        FROM dictionary_entries
        WHERE dict_source_id = ?
        GROUP BY root_normalized
        HAVING COUNT(*) > 1
    """, (mufradat_id,))
    remaining_dupes = cursor.fetchall()

    if remaining_dupes:
        print(f"\n⚠️  WARNING: {len(remaining_dupes)} roots still have duplicates!")
        for root, count in remaining_dupes[:5]:
            print(f"  - {root}: {count} entries")
    else:
        print(f"\n✓ No duplicates remaining - database is clean!")

    conn.close()

    # Run VACUUM to reclaim space
    print(f"\n🗜️  Running VACUUM to reclaim space...")
    conn = sqlite3.connect(db_path)
    conn.execute("VACUUM")
    conn.close()

    new_size_mb = os.path.getsize(db_path) / (1024 * 1024)
    print(f"   New database size: {new_size_mb:.1f} MB (saved {db_size_mb - new_size_mb:.1f} MB)")

if __name__ == '__main__':
    deduplicate_mufradat()
