# Convert Eurasian Phonologies data (JSON) to CSV

curl http://eurasianphonology.info/static/phono_dbase.json | jq '[.[] | {LanguageCode: .code, LanguageName: .name, Segment: .inv[]}]' | in2csv -f json > ../data/raw/phono_dbase.csv

# python3 add_phoible_ids.py > EA_inventories.tsv