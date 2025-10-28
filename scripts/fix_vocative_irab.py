#!/usr/bin/env python3
"""
Fix i'rab for vocative constructions (يا + noun)
Regenerate only affected words to save time
"""

import sqlite3
from generate_irab_from_morphology import IrabGenerator


def get_verses_with_vocatives(db_path: str):
    """Get all verses containing vocative constructions"""
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()

    cursor.execute("""
        SELECT DISTINCT surah, verse
        FROM quranic_words
        WHERE surface_form LIKE 'يَٰٓأَ%'
           OR surface_form LIKE 'يَٰأَ%'
           OR surface_form LIKE 'يَاأَ%'
           OR surface_form LIKE 'يَٰٓ%'
        ORDER BY surah, verse
    """)

    verses = cursor.fetchall()
    conn.close()
    return verses


def main():
    db_path = "../backend/aralex.db"

    print("=" * 80)
    print("Fix Vocative Construction I'rab")
    print("=" * 80)

    # Get verses with vocatives
    verses = get_verses_with_vocatives(db_path)
    print(f"\nFound {len(verses)} verses with vocative constructions (يا + noun)\n")

    # Initialize generator
    generator = IrabGenerator(db_path)

    # Process each verse
    processed = 0
    errors = 0

    for surah, verse in verses:
        try:
            # Regenerate i'rab
            irab_list = generator.generate_verse_irab(surah, verse)

            # Delete old entries
            conn = generator.conn
            cursor = conn.cursor()
            cursor.execute(
                "DELETE FROM irab_details WHERE surah = ? AND verse = ?", (surah, verse)
            )

            # Save new entries
            generator.save_to_database(surah, verse, irab_list)

            processed += 1

            if processed % 20 == 0:
                print(
                    f"Progress: {processed}/{len(verses)} verses - Last: {surah}:{verse}"
                )

        except Exception as e:
            errors += 1
            print(f"ERROR processing {surah}:{verse}: {e}")
            continue

    print("\n" + "=" * 80)
    print("Vocative I'rab Fix Complete!")
    print("=" * 80)
    print(f"Verses updated: {processed}")
    print(f"Errors: {errors}")
    print(
        f"Success rate: {(processed / (processed + errors)) * 100:.1f}%"
        if (processed + errors) > 0
        else "N/A"
    )
    print("\n✅ Vocative constructions now have proper two-part i'rab!")


if __name__ == "__main__":
    main()
