import csv

bibs = {}
with open('Aus_phonemes_needs_final_checking_still_out.csv') as csvfile:
    csv = csv.reader(csvfile)
    for row in csv:
        for refid in row:
            bibs.setdefault(refid, requests.get('http://glottolog.org/resource/reference/id/320043.bib').text)

with open('refs.bib', 'w') as fp:
    fp.write('\n'.join(bibs.values()))
