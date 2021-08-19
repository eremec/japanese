from anki_export import ApkgReader
import pyexcel_xlsxwx

with ApkgReader('Kanji_Radical_Primitive.apkg') as apkg:
    pyexcel_xlsxwx.save_data('radical.xlsx', apkg.export(), config={'format': None})
