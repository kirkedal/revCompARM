.PHONY: all clean

fslex=mono fslex.exe
fsyacc=mono fsyacc.exe
powerpack=FSharp.PowerPack.dll
fsharpc=fsharpc --nologo

LexerGen=Lexer.fs
ParserGen=Parser.fs
AbSynLib=AbSyn.dll
ParserLib=Parser.dll
LexerLib=Lexer.dll
Translate=Translate.fs

all: $(Translate)

$(LexerGen): Lexer.fsl
	$(fslex) Lexer.fsl

$(ParserGen): Parser.fsp
	$(fsyacc) -v --module Parser Parser.fsp

$(AbSynLib): AbSyn.fs
	$(fsharpc) -a AbSyn.fs

$(ParserLib): $(ParserGen) $(AbSynLib)
	$(fsharpc) -a $(ParserGen) -r $(AbSynLib) -r $(powerpack)

$(LexerLib): $(LexerGen) $(AbSynLib) $(ParserLib)
	$(fsharpc) -a $(LexerGen) -r $(AbSynLib) -r $(ParserLib) -r $(powerpack)

$(Translate): $(LexerLib) $(AbSynLib) $(ParserLib)
	$(fsharpc) -r $(LexerLib) -r $(AbSynLib) -r $(ParserLib) -r $(powerpack) $(Translate)

clean:
	rm -f Translate.exe $(LexerLib) $(AbSynLib) $(ParserLib) $(LexerGen) $(ParserGen) Parser.fsi
