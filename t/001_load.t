# -*- perl -*-

# t/001_load.t - check module loading and create testing directory

use Test::More tests => 2;

BEGIN { use_ok( 'Text::Ngram::LanguageDetermine' ); }

my $object = Text::Ngram::LanguageDetermine->new ();
isa_ok ($object, 'Text::Ngram::LanguageDetermine');


