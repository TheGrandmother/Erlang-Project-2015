# Namn på projektet

OSPP (1DT096) 2015 - Grupp 5
Projektarbete på kursen Operativsystem och processorienterad
programmering (1DT096) våren 2015, Uppsala universitet.

## Kompilera

Hela simulationen kompileras igenom att köra `make` i root mappen.
För att kompilera enskilda moduler körs `make` i respektive moduls mapp i `src`
katalogen.

## Testa

Hela systemet testats igenom att köra  `make test` i root mappen
För att testa enskilda moduler körs `make test` i dess egna mapp i `src` katalogen


## Starta systemet

TBI

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
