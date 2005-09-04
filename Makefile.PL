
use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME         => 'Text::Ngram::LanguageDetermine',
    VERSION_FROM => 'lib/Text/Ngram/LanguageDetermine.pm', # finds \$VERSION
    AUTHOR       => 'Rusty Conover (rconover@infogears.com)',
    ABSTRACT     => 'Guess the language of text using ngrams',
    PREREQ_PM    => {
                     'Test::Simple' => 0.44,
		     'Text::Ngram' => 0,
		     'List::Util' => 0,
		     'Storable' => 0,
		     'Params::Check' => 0,
		     'Carp' => 0,
                    },
);
