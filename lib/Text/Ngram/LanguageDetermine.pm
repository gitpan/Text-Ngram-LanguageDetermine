package Text::Ngram::LanguageDetermine;
use strict;

BEGIN {
    use Exporter ();
    use vars qw ($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
    $VERSION     = 0.01;
    @ISA         = qw (Exporter);
    #Give a hoot don't pollute, do not export more than needed by default
    @EXPORT      = qw ();
    @EXPORT_OK   = qw ();
    %EXPORT_TAGS = ();
}


########################################### main pod documentation begin ##


=head1 NAME

Text::Ngram::LanguageDetermine - Guess the language of text using ngrams

=head1 SYNOPSIS

  use Text::Ngram::LanguageDetermine;

NOTE: First build some language profiles using source text, (easily
obtained from places like Wikipedia or else where, subject matter
really doesn't matter but, it should all be in the target language and
saved in UTF-8 format).

my %lang_profiles  = (
   english => create_language_profile(source_filename => 'english.txt'),
   french => create_language_profile(source_filename => 'french.txt'),
   german => create_language_profile(source_filename => 'german.txt'),
);

Get the profile of the text we're wondering about.

my $text_profile = make_text_profile(source_filename => 'query.txt');

Score the text profile against all of the language profiles.

my %scores = map { 
   $_ => compare_profiles(language_profile => $language_profiles{$_},
                          text_profile => $text_profile) 
   } keys %lang_profiles;


The score thats the smallest is the most likely answer, the score
values themselves aren't actually relevant, just the ordering of the
scores. lowest score = most likey, highest score = most unlikely.

print "Language is: " . (sort { $scores{$a} <=> $scores{$b} } keys $scores)[0] . "\n";



=head1 DESCRIPTION

This module performs the task of guessing what language a document is
written in using an ngram profile of a large sample text of each
language and the query text.

It does this by calculating the most frequent ngrams of the sample
text for a language, ranking them by frequency then only keeping the
most popular ngrams removing most subject specific ngrams.  Then it
compares the positions of the ngrams from the language sample text to
the positions of the ngrams from the query text ranked by frequency to
produce a score that indiciates the "Out of Place" measure.  This
measure determines how much the query text's ngrams are out of place
with regard to a languages ngrams.

The language that produces the lowest "Out of Place" measure its most
likely the language the text is written in.

This module was written after reading the paper "N-Gram-Based Text
Categorization" by William B. Cavnar and John M. Trenkle, see:
L<http://citeseer.csail.mit.edu/68861.html>

=head1 BUGS

Might be some.

=head1 SUPPORT

Please contact the author with any patches, bug reports via email.

=head1 AUTHOR

	Rusty Conover
	CPAN ID: RCONOVER
	InfoGears Inc.
	rconover@infogears.com
	http://www.infogears.com

=head1 COPYRIGHT

Copyright 2005 InfoGears Inc.  L<http://www.infogears.com> All Rights
Reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

The full text of the license can be found in the LICENSE file included
with this module.

=head1 SEE ALSO

perl(1). L<Text::Ngram>

=cut

use Text::Ngram;
use List::Util;
use Storable;
use Params::Check;
use Carp;

# Get the data from either a file or a scalar and run the initial
# filter on it to remove any punctuation or digits.

sub _get_source_data {
  my %args = @_;

  my $params_template = {
    source_filename => { required => 0, defined => 1},
    source_data => { required => 0, defined => 1 },
  };
  Params::Check::check($params_template, \%args) || Carp::confess("Bad arguments: " . Params::Check::last_error());
  
  my $data;

  if (defined($args{source_filename})) {
    open(DATAIN, "<" . $args{source_filename}) || Carp::confess("Failed to open $args{source_filename}");
    # Mark the file has being encoded in utf8.
    binmode(DATAIN, ":utf8");    
    {
      local $/ = undef;
      $data = <DATAIN>;
    };
    close(DATAIN);    
  } else {
    $data = $args{source_data};
  }

  # Strip all digits and punctuation.
  $data =~ s/(\d|\p{IsPunct})/ /g;
  
  return $data;
}

=head2 create_language_profile

 Usage : create_language_profile(source_filename => 'english.sample',
 destination_filename => 'english.profile', frequency_cutoff => 300,
 ngram_max_length => 5).

 Purpose : This function creates a language profile for future
 comparision to text, its best to pass in a good 10k to 20k byte
 sample of the language.  It reads that data, creates ngrams of
 various lengths from 1 to ngram_max_length and calculates the
 frequency of each ngram throughout the entire text.  After all of the
 ngrams have been created and sorted by their frequency it truncates
 the list to frequency_cutoff entries.

 The frequency_cuttoff serves the purpose of only keeping the ngrams
 that really aren't subject specific to the text, 300 seems to be a
 good default but its open to tuning.

 ngram_max_length is the maximum length of a ngram to be generated.
 Again this length is open to tuning, but 5 characters seems to be a
 good number.


 Returns : This function returns a hash referench of ngrams with
 ngrams as the key and their frequency rank as the value.

 Argument  :

 source_filename - the filename where to read the source text

 source_data - a scalar that contains the source text

 destination_filename - the filename where to store the profile using
 Storable.

 frequency_cutoff - the cutoff frequency of the ngrams

 ngram_max_length - the maximum length of the ngrams.

 Throws    : uses Carp::confess to complain about errors.

=cut

sub create_language_profile {
  my %args = @_;

  my $params_template = {
    source_filename => { required => 0, defined => 1},
    source_data => { required => 0, defined => 1 },
    destination_filename => { required => 0, defined => 1},

    frequency_cutoff => { required => 0, defined => 1, allow => qr/^\d+/},    
    ngram_max_length => { required => 0, defined => 1, allow => qr/^\d+/},
  };
  Params::Check::check($params_template, \%args) || Carp::confess("Bad arguments: " . Params::Check::last_error());

  if (!defined($args{source_filename}) && !defined($args{source_data})) {
    Carp::confess("Neither source_filename nor source_data passed to obtain language data.");
  }

  $args{frequency_cutoff} ||= 300;
  $args{ngram_max_length} ||= 5;

  
  my $all_ngrams_count = _make_text_ngrams(map { 
    $_ => $args{$_}
  } grep !/(frequency_cutoff|destination_filename)$/, keys %args);

  # Sort all of the ngrams by frequency.
  my @sorted_keys = sort { $all_ngrams_count->{$b} <=> $all_ngrams_count->{$a} } sort keys %$all_ngrams_count;

  # Rank the ngrams with the most frequency used starting with ngram
  # at zero to the least frequency used, the reason this is a hash is
  # becauase its a faster lookup then just an array when we want to
  # guess what language text is in.
  
  my $u = 1;
  my %rank = map { $_ => $u++ } @sorted_keys[0..List::Util::min($args{frequency_cutoff}, scalar(@sorted_keys)-1)];

  if (defined($args{destination_filename})) {
    Storable::nstore(\%rank, $args{destination_filename}) || Carp::confess("Failed to store ngram profile to file: $args{destination_filename}");
  }

  return \%rank;  
}

=head2 create_text_profile

 Usage : create_text_profile(source_filename => 'interesting.txt',
 ngram_max_length => 5)

 Purpose : This function creates the comparison profile for an
 arbitrary piece of text, it calculates all of the ngrams for the text
 and then sorts them by frequency.

 Returns : An array reference containing the ngrams sorted by
 frequency of occurrance in the passed text.


 Argument  : 

 source_filename - The filename where to read the source text

 source_data - The data to use as the source passed as a scalar

 ngram_max_length - The maximum ngram length

 Throws    : uses Carp::confess for errors.

=cut


sub create_text_profile {
  my %args = @_;

  my $params_template = {
    source_filename => { required => 0, defined => 1},
    source_data => { required => 0, defined => 1 },
    ngram_max_length => { required => 0, defined => 1, allow => qr/^\d+/},
  };
  Params::Check::check($params_template, \%args) || Carp::confess("Bad arguments: " . Params::Check::last_error());

  my $all_ngrams_count = _make_text_ngrams(%args);

  my @sorted_keys = sort { $all_ngrams_count->{$b} <=> $all_ngrams_count->{$a} } sort keys %$all_ngrams_count;

  return \@sorted_keys;
}

=head2 compare_profiles

 Usage : compare_profiles(comparison_profile
 => $compare_profile, language_profile => $lang_profile)

 Purpose : This function compares a language profile to a text profile
 and calculates a score determining if the text's ngram frequency
 matches well with the language's frequency.  This is called the
 "Out-of-Place" measure.


 Returns : An integer score measuring how much the ngrams in the text
 profile are out of place with the ngrams in the language profile.

 Argument  : 

 comparison_profile - the comparison profile created by create_text_profile()

 language_profile - the language profile created by create_language_profile()

 ngram_not_found_distance - the distance value used for ngrams not
 found in the language profile, by default this is 2 * the total
 ngrams in the language profile.


 Throws : uses Carp::confess for bad arguments.

 Comments : To determine which language the text is written in, the
 best guess of this algorithm is the language with the lowest score
 returned by this function.

=cut


sub compare_profiles {
  my %args = @_;

  my $params_template = {
    comparison_profile => { required => 1, defined => 1, strict_type => 1, default => []},
    language_profile => { required => 1, defined => 1, strict_type => 1, default => {}},    
    ngram_not_found_distance => { required => 0, defined => 1},
  };
  Params::Check::check($params_template, \%args) || Carp::confess("Bad arguments: " . Params::Check::last_error());

  # Default the not foudn weight to 2*total ngrams.
  $args{ngram_not_found_distance} ||= scalar(keys %{$args{language_profile}})*2;
  
  my $match_distance = 0;
  for (my $j = 0; $j < scalar(@{$args{comparison_profile}}); $j++) {
    $match_distance += ((exists($args{language_profile}->{$args{comparison_profile}->[$j]})) ? 
			 abs($j-$args{language_profile}->{$args{comparison_profile}->[$j]}) :
			   $args{ngram_not_found_distance});
  }

  return $match_distance;
}


sub _make_text_ngrams {
  my %args = @_;
  my $params_template = {
    source_filename => { required => 0, defined => 1},
    source_data => { required => 0, defined => 1 },

    ngram_max_length => { required => 0, defined => 1, allow => qr/^\d+/},
  };
  Params::Check::check($params_template, \%args) || Carp::confess("Bad arguments: " . Params::Check::last_error());

  $args{ngram_max_length} ||= 5;

  my $data = _get_source_data(map { 
    $_ => $args{$_}
  } grep { exists($args{$_}) } ('source_filename', 'source_data'));


  my $all_ngrams_count = {};

  # Build up a hash containing the unique words from the text and
  # store the word as the key and the value is the frequency.
  
  my %unique_words;
  map { $unique_words{$_}++ } split /\s+/, $data;
  
  my $j =0;
  foreach my $word (keys %unique_words) {  
    # Track the ngrams of this word.
    my $this_word = {};
    
    for (my $i = 1; $i < List::Util::min(length($word)+2, $args{ngram_max_length}+1); $i++) {
      Text::Ngram::add_to_counts(" " . $word . " ", $i, $this_word);
    }

    # Loop over all of the ngrams for this word and increase the
    # frequency of the ngrams into the main count.
    map { 
      $all_ngrams_count->{$_} += $unique_words{$word};
    } grep !/\0/, keys %$this_word;
  } 


  return $all_ngrams_count;
}

1; #this line is important and will help the module return a true value

