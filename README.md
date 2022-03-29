# yukiparse

An improvement upon [this](https://github.com/ephing/dragontrials/tree/main/trial3and4)

I added some CLI flags and also semantic action support into the specification for grammars.

Parsers output from yukiparse are dependent on a lexer such as [yunolex](https://github.com/ephing/yunolex), or any other lexer that outputs token streams in the same format.