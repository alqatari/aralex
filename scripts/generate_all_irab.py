#!/usr/bin/env python3
"""
Generate إعراب for ALL verses with case-marked words
Batch processing script for complete coverage
"""

import sqlite3
import sys
from generate_irab_from_morphology import IrabGenerator


def get_verses_with_case_marking(db_path: str):
    """Get all unique (surah, verse) pairs that have case marking"""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT DISTINCT surah, verse
        FROM quranic_words
        WHERE segments_json LIKE '%CaseDTO%'
        ORDER BY surah, verse
    """)

    verses = cursor.fetchall()
    conn.close()
    return verses


def main():
    db_path = "../backend/aralex.db"

    print("=" * 80)
    print("Batch إعراب Generation - All Verses with Case Marking")
    print("=" * 80)

    # Get all verses
    print("\nFetching verses with case marking...")
    verses = get_verses_with_case_marking(db_path)
    print(f"Found {len(verses)} verses with case-marked words\n")

    # Initialize generator
    generator = IrabGenerator(db_path)

    # Process in batches with progress indicator
    total = len(verses)
    processed = 0
    errors = 0

    for surah, verse in verses:
        try:
            # Generate i'rab
            irab_list = generator.generate_verse_irab(surah, verse)

            # Save to database
            generator.save_to_database(surah, verse, irab_list)

            processed += 1

            # Progress indicator every 100 verses
            if processed % 100 == 0:
                percent = (processed / total) * 100
                print(
                    f"Progress: {processed}/{total} verses ({percent:.1f}%) - Last: {surah}:{verse}"
                )

        except Exception as e:
            errors += 1
            print(f"ERROR processing {surah}:{verse}: {e}", file=sys.stderr)
            continue

    print("\n" + "=" * 80)
    print("Batch Generation Complete!")
    print("=" * 80)
    print(f"Total verses: {total}")
    print(f"Successfully processed: {processed}")
    print(f"Errors: {errors}")
    print(f"Success rate: {(processed / total) * 100:.1f}%")


if __name__ == "__main__":
    main()
