from bs4 import BeautifulSoup

with open('../complete.xml') as xfiles:
    xcnt = xfiles.read()

soup = BeautifulSoup(xcnt)


xmlo = soup.prettify("utf-8")
with open("../nyphil_clean.xml", "wb+") as file:
    file.write(xmlo)