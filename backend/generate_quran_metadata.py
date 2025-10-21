#!/usr/bin/env python3
"""
Generate complete Haskell surah metadata from quran-data.xml
"""

import xml.etree.ElementTree as ET

def parse_xml():
    tree = ET.parse('../data/quran_text/quran-data.xml')
    root = tree.getroot()

    suras_elem = root.find('suras')
    suras = []

    for sura in suras_elem.findall('sura'):
        suras.append({
            'index': int(sura.get('index')),
            'ayas': int(sura.get('ayas')),
            'name': sura.get('name'),
            'tname': sura.get('tname'),
            'ename': sura.get('ename'),
            'type': sura.get('type'),
            'order': int(sura.get('order')),
            'rukus': int(sura.get('rukus'))
        })

    return suras

def escape_string(s):
    """Escape quotes and backslashes in Haskell strings"""
    return s.replace('\\', '\\\\').replace('"', '\\"')

def generate_haskell(suras):
    lines = []
    lines.append('-- | Complete metadata for all 114 surahs (auto-generated from quran-data.xml)')
    lines.append('allSurahs :: [SurahInfo]')
    lines.append('allSurahs =')

    for i, sura in enumerate(suras):
        prefix = '  [ ' if i == 0 else '  , '

        # Build SurahInfo constructor call
        surah_name = f'SurahName "{escape_string(sura["name"])}" "{escape_string(sura["tname"])}" "{escape_string(sura["ename"])}"'

        line = (f'{prefix}SurahInfo '
                f'(SurahNumber {sura["index"]}) '
                f'({surah_name}) '
                f'(VerseCount {sura["ayas"]}) '
                f'{sura["type"]} '
                f'(RevelationOrder {sura["order"]}) '
                f'(RukuCount {sura["rukus"]})')

        lines.append(line)

    lines.append('  ]')
    return '\n'.join(lines)

if __name__ == '__main__':
    suras = parse_xml()
    print(f"-- Parsed {len(suras)} surahs from quran-data.xml")
    print()
    haskell_code = generate_haskell(suras)
    print(haskell_code)
