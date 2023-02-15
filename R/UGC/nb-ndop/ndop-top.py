# pip3 install ndop-downloader # samostatná příkazová řádka
# /home/petr/.local/lib/python3.8/site-packages/ndop/__init__.py # přepsat natvrdo napsaný limit 500 řádků alespoň na 1000 pro večejný přístup

# tento skript volání:
# python3 ndop-top.py --kategorie=14 --date_from="1.1.2010" --date_to="31.12.2021"

import ndop as nd


import argparse, sys
parser=argparse.ArgumentParser()
parser.add_argument('--taxon')
parser.add_argument('--date_from')
parser.add_argument('--date_to')
parser.add_argument('--kategorie')
args=parser.parse_args()

search_payload = nd.get_search_pars()

# 'rfDatumOd':'1.1.2018'
# 'rfKategorie': 11 
    # 11 obojživelníci 174002
    # 16 ryby 29309
    # 17 plazi 52837
    # 12 ptáci 209745 (do 31.12.2011), (((204717 do 31.12.2012)))
    # 13 netopýři 71170
    # 14 savci 90799
    # 53 pavouci 38612
    # 67 vážky 100922
    # 56 brouci 438277
    # 63 motýli 88657 (do 31.12.2011) + (do 31.12.2018) 259402 + 229112 # nejsou unikátná, jsou tam duplicity od/do funguje divně
    # 65 měkkýši 52480
# 'rfTaxon':args.taxon, 
search_payload.update({'rfDatumOd':args.date_from, 'rfDatumDo':args.date_to, 'rfKategorie':args.kategorie})

# přihlašovací údaje zadáme ručně
username = "petr_balej.net"
password = "Kaplan2019"

# nebo můžeme načíst z konfiguračního souboru
# username, password = nd.read_config("~/.ndop.cfg")

s = nd.login(username, password)
table_payload,num_rec = nd.search_filter(s,search_payload)

nd.get_ndop_csv_data(s,
                    num_rec,
                    table_payload,
                    "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/"+args.kategorie+"-"+args.date_from+"-"+args.date_to  # "K:/Downloads/uga/ndop-downloader" "/mnt/2AA56BAE3BB1EC2E/Downloads/uga/ndop-downloader/"
                    )
