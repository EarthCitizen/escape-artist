ctors=$(cat ../src/Text/EscapeArtist/Internal.hs | grep forall | grep ToEscapable | sed -n -E 's/(^.*=> *)([A-Za-z]+)( +.*)/\2/p')

val=1

for ctor in $ctors
do
    printf "($ctor $val, \"$ctor ($val)\"),\n"
    val=$((val+1))
done
