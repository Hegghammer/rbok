#-----------------------------------
# Kode til kapittel 2 i "R for alle"
# Thomas Hegghammer, desember 2023
#-----------------------------------

# [NB: Disse kommandoene er ikke ment å kjøres i R, men i terminalen på henholdsvis Windows, Mac og Linux.]

# Windows =========================

color e5

dir

mkdir temp

cd temp

mkdir min_mappe && cd min_mappe

cd ..

cd ..

cd temp/min_mappe

cd \

cd C:\temp

copy nul test.txt

echo Godt jobbet! > test.txt

echo Hello world!

type test.txt

move test.txt min_mappe

notepad

notepad test.txt

notepad C:\temp\min_mappe\test.txt

del C:\temp\min_mappe\test.txt

rmdir min_mappe

cd .. && rmdir temp

# Mac ==================================

pwd

ls

mkdir temp

cd temp

mkdir min_mappe && cd min_mappe

cd ..

cd temp/min_mappe

cd \

cd /Users/BRUKERNAVN/temp

touch test.txt

echo Godt jobbet! > test.txt

echo Hello World!

cat test.txt

mv test.txt min_mappe

open -a TextEdit

open -a TextEdit /Users/BRUKERNAVN/temp/min_mappe/test.txt

rm /Users/BRUKERNAVN/temp/min_mappe/test.txt

cd /Users/BRUKERNAVN/temp

rmdir min_mappe  # Eller rm -rf min_mappe

cd .. && rmdir temp

# Linux ==============================

gedit

gedit /home/BRUKERNAVN/temp/test.txt
