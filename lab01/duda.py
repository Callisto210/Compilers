import os
import sys
import re
import codecs
import datetime

#Aby uniknac sytuacji, gdzie w tekscie moze trafic sie znacznik meta, musimy najpierw
#wyciagnac meta z poczatku pliku

def get_meta(content):
	return '\n'.join(re.compile('^<META NAME.+$', re.MULTILINE).findall(content))

#Analogicznie wyciagamy sobie reszte slow

def get_other(content):
	return '\n'.join(re.compile('<P>(.+?)</P>', re.DOTALL).findall(content))


def processFile(filepath):
    fp = codecs.open(filepath, 'rU', 'iso-8859-2')

    content = fp.read()
    meta = get_meta(content)
    
    autor_regexp = re.compile('<META NAME="AUTOR" CONTENT="(.+)">')
    autor = autor_regexp.findall(meta)
    
    dzial_regexp = re.compile('<META NAME="DZIAL" CONTENT="(.+)">')
    dzial = dzial_regexp.findall(meta)
    dzial = [d.split("/")[1] for d in dzial]
    
    slowa_regexp = re.compile('<META NAME="KLUCZOWE_[0-9]*" CONTENT="(.+)">')
    slowa = slowa_regexp.findall(meta)
    
    other = get_other(content)
    
#    print other
    #\d\d (\d)\1
    skroty_regexp = re.compile(r'\s[^\W\d]{1,3}\.\s')
    skroty = skroty_regexp.findall(other)
    other = skroty_regexp.sub('', other)
    
    mail_regexp = re.compile(r'[a-z0-9_-]+(?:\.[a-z0-9_-]+)*@(?:[a-z0-9_-]+\.)+[a-z]{2,4}')
    mail = mail_regexp.findall(other)
    other = mail_regexp.sub('', other)
    
    daty = set()
    daty_pl_regexp = re.compile(r'(?:(?:0[1-9]|[12][0-9])-02-\d{4})|(?:(?:0[1-9]|[12][0-9]|3[01])-(?:0[13578]|1[02])-\d{4})|(?:(?:0[1-9]|[12][0-9]|30)-(?:0[469]|11)-\d{4})|(?:(?:0[1-9]|[12][09])/02/\d{4})|(?:(?:0[1-9]|[12][0-9]|3[01])/(?:0[13578]|1[02])/\d{4})|(?:(?:0[1-9]|[12][0-9]|30)/(?:0[469]|11)/\d{4})|(?:(?:0[1-9]|[12][0-9])\.02\.\d{4})|(?:(?:0[1-9]|[12][0-9]|3[01])\.(?:0[13578]|1[02])\.\d{4})|(?:(?:0[1-9]|[12][0-9]|30)\.(?:0[469]|11)\.\d{4})')
    daty_pl = daty_pl_regexp.findall(other)
    other = daty_pl_regexp.sub('', other)
    
    daty_us_regexp = re.compile(r'(?:\d{4}\.(?:0[1-9]|[12][0-9])\.02)|(?:\d{4}\.(?:0[1-9]|[12][0-9]|3[01])\.(?:0[13578]|1[02]))|(?:\d{4}\.(?:0[1-9]|[12][0-9]|30)\.(?:0[469]|11))|(?:\d{4}-(?:0[1-9]|[12][0-9])-02)|(?:\d{4}-(?:0[1-9]|[12][0-9]|3[01])-(?:0[13578]|1[02]))|(?:\d{4}-(?:0[1-9]|[12][0-9]|30)-(?:0[469]|11))|(?:\d{4}/(?:0[1-9]|[12][0-9])/02)|(?:\d{4}/(?:0[1-9]|[12][0-9]|3[01])/(?:0[13578]|1[02]))|(?:\d{4}/(?:0[1-9]|[12][0-9]|30)/(?:0[469]|11))')
    daty_us = daty_us_regexp.findall(other)
    other = daty_us_regexp.sub('', other)
    
    for d in daty_pl:
	    daty_pl_conv = re.sub(r'[\./]', '-', d)
	    if daty_pl_conv:
		    daty.add(datetime.datetime.strptime(daty_pl_conv, '%d-%m-%Y'))
		    
    for d in daty_us:
        daty_us_conv = re.sub(r'[\./]', '-', d)
        if daty_us_conv:
            daty.add(datetime.datetime.strptime(daty_us_conv, '%Y-%d-%m'))
    
    float_regexp = re.compile(r'\b-?\d*\.(?:\de[+-]\d+|\d+)\b')
    float_var = float_regexp.findall(other)
    other = float_regexp.sub('', other)
    
    integer_regexp = re.compile(r'(?:(?:\s|\D)-32768(?:\s|\D))|(?:\s-?3276[0-7]\s)|(?:\s-?327[0-5][0-9]\s)|(?:\s-?32[0-6][0-9]{2}\s)|(?:\s-?3[0-1][0-9]{3}\s)|(?:\s-?[1-2][0-9]{4}\s)|(?:\s-?[1-9][0-9]{,3}\s)|(?:\s0\s)')
    integer = integer_regexp.findall(other)
    
    
    #dzieki uzyciu metody sub pozbylem sie innych rzeczy z tekstu ktore moga przeszkadzac
    #Zdanie zaczynamy z wielkiej litery i konczymy kropka
    sentence_regexp = re.compile(r'[A-Z][ \S]+?[\.?!]+')
    sentence = sentence_regexp.findall(other)
    




    fp.close()
    print("nazwa pliku:", filepath)
    print("autor:", ", ".join(autor))
    print("dzial:", ", ".join(dzial))
    print("slowa kluczowe:", ", ".join(slowa))
    print("liczba zdan:", len(sentence))
    print("liczba skrotow:", len(set(skroty)))
    print("liczba liczb calkowitych z zakresu int:", len(set(integer)))
    print("liczba liczb zmiennoprzecinkowych:", len(set(float_var)))
    print("liczba dat:", len(daty))
    print("liczba adresow email:", len(set(mail)))
    print("\n")



try:
    path = sys.argv[1]
except IndexError:
    print("Brak podanej nazwy katalogu")
    sys.exit(0)

tree = os.walk(path)

for root, dirs, files in tree:
    for f in files:
        if f.endswith(".html"):
            filepath = os.path.join(root, f)
            processFile(filepath)



