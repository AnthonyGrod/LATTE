Opis:

Pliki zrodlowe znajdują się w katalogu `src` natomiast w katalogu `lib/Parser` znajdują się pliki wygenerowane przez bibliotekę `bnfc`

W katalogu `lib` znajdują się pliki, które zostały wykorzystane do generacji pomocniczych
funkcji runtime.

Ponadto program korzysta z bibliotek mtl, containers, array, process. 

Zaimplementowane optymalizacje: LCSE, Zmienne indukcyjne i redukcja mocy.

Uruchomienie:
```
make
./latc file
```
