import requests
from bs4 import BeautifulSoup
import os

url_berlin = 'https://www.airbnb.de/rooms/49844243/photos?check_in=2021-12-13&check_out=2021-12-19&federated_search_id=963d4b11-bea9-49e1-a65a-0459f9a7c851&source_impression_id=p3_1638369605_5UVHasmGvTz3j1K1&guests=1&adults=1'
def imagedown(url, folder):
    try:
        os.mkdir(os.path.join(os.getcwd(), folder))
    except:
        pass
    os.chdir(os.path.join(os.getcwd(), folder))
    r = requests.get(url)
    soup = BeautifulSoup(r.text, 'html.parser')
    images = soup.find_all('img')
    for image in images:
        name = image['alt']
        link = image['src']
        with open(name.replace(' ', '-').replace('/', '') + '.jpg', 'wb') as f:
            im = requests.get(link)
            f.write(im.content)
            print('Writing: ', name)

imagedown(url_berlin, 'berlin')
