#!/usr/bin/env python

import sys
import os

AdditionalHK = """
79\t30FC
7A\t3002
7B\t300C
7C\t300D
7D\t3001
7E\t30FB
"""

AdditionalHiragana = "77\t309D\n78\t309E" + AdditionalHK

AdditionalKatakana = "77\t30FD\n78\t30FE" + AdditionalHK

if __name__ == '__main__':
  DIR = os.path.dirname(os.path.dirname(sys.argv[0]))
  os.chdir(DIR)

  with open("Kanji.txt", 'w')       as kanji,\
       open("Eisuu.txt", 'w')       as eisuu,\
       open("Katakana.txt", 'w')    as katakana,\
       open("Hiragana.txt", 'w')    as hiragana,\
       open("JisKatakana.txt", 'w') as jisKatakana,\
       open("JisKanji1.txt", 'w')   as jisKanji1,\
       open("JisKanji2.txt", 'w')   as jisKanji2,\
       open("Arib.txt", 'w')        as arib:

    for line in open("src/JIS0208.TXT", 'r'):
      tmp = line.split('#')[0].split('\t')
      if len(tmp) >= 3:
        jis = int(tmp[1][2:], 16)
        uni = tmp[2][2:]

        kanji.write("{0:X}\t{1}\n".format(jis, uni))

        if 0x2520 < jis <= 0x2576:
          katakana.write("{0:X}\t{1}\n".format(jis - 0x2500, uni))

        if 0x2420 < jis <= 0x2473:
          hiragana.write("{0:X}\t{1}\n".format(jis - 0x2400, uni))

    katakana.write(AdditionalKatakana)
    hiragana.write(AdditionalHiragana)

    for line in open("src/JIS0201.TXT", 'r'):
      tmp = line.split('#')[0].split('\t')
      if len(tmp) >= 2:
        jis = int(tmp[0][2:], 16)
        uni = tmp[1][2:]

        if jis < 0x80:
          eisuu.write("{0:X}\t{1}\n".format(jis, uni))

        if jis > 0xA0:
          jisKatakana.write("{0:X}\t{1}\n".format(jis - 0x80, uni))

    for line in open("src/jisx0213-2004-std.txt", 'r'):
      tmp = line.split('#')[0].split('\t')
      if len(tmp) >= 2 and tmp[1]:
        [field, code] = tmp[0].split("-")
        unicodes = tmp[1][2:].split('+')
        linestr  = "{0}\t{1}\n".format(code, '\t'.join(unicodes))
        if field == '3':
          jisKanji1.write(linestr)
        if field == '4':
          jisKanji2.write(linestr)

    for line in open("src/arib.txt", 'r'):
      tmp = line.split('#')[0].split('\t')
      if len(tmp) >= 2 and tmp[1]:
        code = tmp[0][2:]
        unicodes = tmp[1][2:].split('+')
        linestr = "{0}\t{1}\n".format(code, '\t'.join(unicodes))
        kanji.write(linestr)
        arib.write(linestr)

