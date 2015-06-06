# A bug's life

OSPP (1DT096) 2015 - Grupp 5
Projektarbete på kursen Operativsystem och processorienterad
programmering (1DT096) våren 2015, Uppsala universitet.

## Beroenden

För att systemet skall kunna kompileras och för att köra ascii gui:t så kärvs att 
Erlang/OTP 17 finns och att Eshel är version 6.2 eller hörgre.

För att kunna köra Python Guit krävs att följade är instalerat 

* [Python 3.x](https://www.python.org/downloads/)
* [Erlport 1.0.0alpha](http://erlport.org/)
* [Pygame](http://www.pygame.org/)

## Kompilera

Skriv `make <module>` för att kompilera en modul där `<module>` är namnet på modulen.

Eller skriv `make all` för att kompilera hela projektet.

## Testa

Hela systemet testats igenom att köra  `make test` i root mappen. ***OBS*** Bygg beroenderna fungerar inte riktigt så man kan behöva köra `make test` en extra gång för att testerna skall starta.
Skriv `make test_<module>` för att för att testa en enskild modul där `<module>` är namnet på modulen.

## Starta systemet

För att starta ascii gui:t så räcker det mad att skriva `make run_ascii`.

För att starta Python gui:t så måste man manuelt gå in i `ebin/` mappen. Starta `erl` och skriva `gui:initGui().`.
Detta då Pythonmodulerna inte laddas korrekt annars.

## Git policy

### Branches
Varje ny feature skall finnas i en branch under som heter `features/xxxxx`.
alla som arbetar med en feature arbetar sen på den featurn i den branchen och när featuren
är klar så skall ett pull request skapas.

Tänk på att det kan vara en bra ide att skapa en egen fork utav hela repot om man ska hålla på och hoppa runt mellan datorer mycket.

#### Merges
En merge får endast ske när:
* En feature är "helt" klar.
* När det finns tillfredställande tester (Som äver passar såklart ^^)
* Efter att någon har reviewat koden
* När koden är väl dokumenterad.

### Commits
Endast commits till ens egen dagbok och mötesprotokoll får göras och skall göras direkt till `master`.

### Rebase
Git komandot `rebase` är väldigt bra men väldigt läskigt.
`rebase` används för att skriva om historiken. Detta kan användas för att få till mindre fruktansvärda commit loggar.

Det lättaste sättet att uppnå detta är att alltif köra `git pull --rebase` istället för att bara pulla.
Detta gör så att alla ens icke pushade commits kommer att hamna "sist" i historiken. Detta skapar en finare struktur som är mer lättläst.

Sen kommer själva `git rebase --interactive` kommandot in i bilden. Men vi går igenom detaljerna för det sedan då det är lätta att fucka upp totalt :)
