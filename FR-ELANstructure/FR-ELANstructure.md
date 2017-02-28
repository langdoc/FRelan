# ELAN structure standard used by the Freiburg Research Group in Saami Studies

***

|name|parent|type|stereotype|obligatory|LANG_REF attribute|purpose|
|:---|:-----|:---|:---------|:--------:|:----------------:|:------|
|ref@|root|refT|time-aligned|x| |Uniquely identifies annotation (utterance for a speaker); pattern: .0024?|
|.orth@|ref|orthT|symb.-assoc.|x|x|Standard orthography; input for FST-script|
|..word@|orth|wordT|symb.-subdiv.|x|x|Wordform; Extracted by FST-script; input for FST|
|...lemma@|word|lemmaT|symb.-subdiv.|x| |Lists lemma/possible lemmas; output from FST |
|....pos@|lemma|posT|symb.-subdiv.|x| |Lists part of speech for lemma; output from FST|
|.....morph@|pos|morphT|symb.-subdiv.|x| |Lists values for morphological categories (Giellatekno-style); output from FST|
|.....gloss@|pos|glossT|symb.-assoc.| | |English gloss for each PoS of each lemma; extracted from sje lexical database; currently only used for sje|
|..ft-lang@|orth|ft-langT|symb.-assoc.| | |Free translation of orth; ‘lang’ is replaced with specific language (eng=english, deu=deutsch -- use ISO-codes?); can have multiple derivations|
|..orth-orig@|orth|orth-origT|symb.-assoc.| | |Orthography used in the original source|



***

##Rules:

+ Each ref annotation must have content (which content?) and it has to be unique (usually a unique number identifier such as .001)
+ Each tier (except for general notes tier?) must have participant, uniquely identified after @ in tier name; standard for naming participant can be project-specific


***

##Things to do:
+ Niko: add orig-convention to places where it is needed
+ Anyone: come up with some solution to resolve note-tiers, for example three types assigned to currently existing (check this) notes:
+ what to do with tiers for annotations with notes
⋅⋅+note-refT
⋅⋅+note-orthT
⋅⋅+note-wordT


***
#####[The Freiburg Research Group in Saami Studies](http://saami.uni-freiburg.de)