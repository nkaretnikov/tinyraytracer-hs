.PHONY: build clean

FILE=Main

build:
	ghc -Wall -Werror -fforce-recomp -O2 -o ${FILE} ${FILE}.hs

clean:
	rm ${FILE}
