package Getopt::Long::EvenLess;

# DATE
# VERSION

# let's be minimalistic
#use strict 'subs', 'vars';
#use warnings;

our @EXPORT   = qw(GetOptions);
our @EXPORT_OK = qw(GetOptionsFromArray);

my $config = {
    pass_through => 0,
};

sub Configure {
    my $old_config = { %$config };

    if (ref($_[0]) eq 'HASH') {
        for (keys %{$_[0]}) {
            $config->{$_} = $_[0]{$_};
        }
    } else {
        for (@_) {
            if ($_ eq 'pass_through') {
                $config->{pass_through} = 1;
            } elsif ($_ eq 'no_pass_through') {
                $config->{pass_through} = 0;
            } else {
                die "Unknown configuration '$_'";
            }
        }
    }
    $old_config;
}

sub import {
    my $pkg = shift;
    my $caller = caller;
    my @imp = @_ ? @_ : @EXPORT;
    for my $imp (@imp) {
        if (grep {$_ eq $imp} (@EXPORT, @EXPORT_OK)) {
            *{"$caller\::$imp"} = \&{$imp};
        } else {
            die "$imp is not exported by ".__PACKAGE__;
        }
    }
}

sub GetOptionsFromArray {
    my ($argv, %spec) = @_;

    my $success = 1;

    my %spec_by_opt_name;
    for (keys %spec) {
        my $orig = $_;
        s/=[fios]\@?\z//;
        s/\|.+//;
        $spec_by_opt_name{$_} = $orig;
    }

    my $code_find_opt = sub {
        my ($wanted, $short_mode) = @_;
        my @candidates;
      OPT_SPEC:
        for my $spec (keys %spec) {
            $spec =~ s/=[fios]\@?\z//;
            my @opts = split /\|/, $spec;
            for my $o (@opts) {
                next if $short_mode && length($o) > 1;
                if ($o eq $wanted) {
                    # perfect match, we immediately go with this one
                    @candidates = ($opts[0]);
                    last OPT_SPEC;
                } elsif (index($o, $wanted) == 0) {
                    # prefix match, collect candidates first
                    push @candidates, $opts[0];
                    next OPT_SPEC;
                }
            }
        }
        if (!@candidates) {
            unless ($config->{pass_through}) {
                warn "Unknown option: $wanted\n";
                $success = 0;
            }
            return undef; # means unknown
        } elsif (@candidates > 1) {
            unless ($config->{pass_through}) {
                warn "Option $wanted is ambiguous (" .
                    join(", ", @candidates) . ")\n";
                $success = 0;
            }
            return ''; # means ambiguous
        }
        return $candidates[0];
    };

    my $code_set_val = sub {
        my $name = shift;

        my $spec_key = $spec_by_opt_name{$name};
        my $handler  = $spec{$spec_key};

        $handler->({name=>$name}, @_ ? $_[0] : 1);
    };

    my $i = -1;
    my @remaining;
  ELEM:
    while (++$i < @$argv) {
        if ($argv->[$i] eq '--') {

            push @remaining, @{$argv}[$i+1 .. @$argv-1];
            last ELEM;

        } elsif ($argv->[$i] =~ /\A--(.+?)(?:=(.*))?\z/) {

            my ($used_name, $val_in_opt) = ($1, $2);
            my $opt = $code_find_opt->($used_name);
            if (!defined($opt)) {
                # unknown option
                push @remaining, $argv->[$i];
                next ELEM;
            } elsif (!length($opt)) {
                next ELEM; # ambiguous
            }

            my $spec = $spec_by_opt_name{$opt};
            # check whether option requires an argument
            if ($spec =~ /=[fios]\@?\z/) {
                if (defined $val_in_opt) {
                    # argument is taken after =
                    $code_set_val->($opt, $val_in_opt);
                } else {
                    if ($i+1 >= @$argv) {
                        # we are the last element
                        warn "Option $used_name requires an argument\n";
                        $success = 0;
                        last ELEM;
                    }
                    $i++;
                    $code_set_val->($opt, $argv->[$i]);
                }
            } else {
                $code_set_val->($opt);
            }

        } elsif ($argv->[$i] =~ /\A-(.*)/) {

            my $str = $1;
          SHORT_OPT:
            while ($str =~ s/(.)//) {
                my $used_name = $1;
                my $opt = $code_find_opt->($1, 'short');
                next SHORT_OPT unless defined($opt) && length($opt);

                my $spec = $spec_by_opt_name{$opt};
                # check whether option requires an argument
                if ($spec =~ /=[fios]\@?\z/) {
                    if (length $str) {
                        # argument is taken from $str
                        $code_set_val->($opt, $str);
                        next ELEM;
                    } else {
                        if ($i+1 >= @$argv) {
                            # we are the last element
                            warn "Option $used_name requires an argument\n";
                            $success = 0;
                            last ELEM;
                        }
                        # take the next element as argument
                        $i++;
                        $code_set_val->($opt, $argv->[$i]);
                    }
                } else {
                    $code_set_val->($opt);
                }
            }

        } else { # argument

            push @remaining, $argv->[$i];
            next;

        }
    }

  RETURN:
    splice @$argv, 0, ~~@$argv, @remaining; # replace with remaining elements
    return $success;
}

sub GetOptions {
    GetOptionsFromArray(\@ARGV, @_);
}

1;
#ABSTRACT: Like Getopt::Long::Less, but with even less features

=head1 DESCRIPTION

This module (GLEL for short) is a reimplementation of L<Getopt::Long> (GL for
short), but with much less features. It's an even more stripped down version of
L<Getopt::Long::Less> (GLL for short) and is perhaps less convenient to use for
day-to-day scripting work.

The main goal is minimum amount of code and small startup overhead. This module
is an experiment of how little code I can use to support the stuffs I usually do
with GL.

Compared to GL and GLL, it:

=over

=item * minimum Configure() support

Only these configurations are known: pass_through, no_pass_through (default).

GLEL is equivalent to GL in this mode: bundling, no_ignore_case,
no_getopt_compat, gnu_compat, permute.

No support for configuring via import options e.g.:

 use Getopt::Long qw(:config pass_through);

=item * does not support increment (C<foo+>)

=item * no type checking (C<foo=i>, C<foo=f>, C<foo=s> all accept any string)

=item * does not support optional value (C<foo:s>), only no value (C<foo>) or required value (C<foo=s>)

=item * does not support desttypes (C<foo=s@>)

=item * does not support handler other than coderef (so no C<< "foo=s" => \$scalar >>, C<< "foo=s" => \@ary >>, only C<< "foo=s" => sub { ... } >>)

Also, in coderef handler, code will get a simple hash instead of a "callback"
object as its first argument.

=item * does not support hashref as first argument

=item * does not support bool/negation (no C<foo!>, so you have to declare both C<foo> and C<no-foo> manually)

=back

The result?

B<Amount of code>. GLEL 0.07 is about 175 lines of code, while GL is about 1500.
Sure, if you I<really> want to be minimalistic, you can use this single line of
code to get options:

 @ARGV = grep { /^--([^=]+)(=(.*))?/ ? ($opts{$1} = $2 ? $3 : 1, 0) : 1 } @ARGV;

and you're already able to extract C<--flag> or C<--opt=val> from C<@ARGV> but
you also lose a lot of stuffs like autoabbreviation, C<--opt val> syntax support
syntax (which is more common, but requires you specify an option spec), custom
handler, etc.

B<Startup overhead>. Here's a sample startup overhead benchmark:

# COMMAND: perl devscripts/bench-startup 2>&1


=head1 FUNCTIONS

=head2 Configure(@configs | \%config) => hash

Set configuration. Known configurations:

=over

=item * pass_through

Ignore errors (unknown/ambiguous option) and still make GetOptions return true.

=item * no_pass_through (default)

=back

Return old configuration data. To restore old configuration data you can pass it
back to C<Configure()>, e.g.:

 my $orig_conf = Getopt::Long::EvenLess::Configure("pass_through");
 # ...
 Getopt::Long::EvenLess::Configure($orig_conf);

=head2 GetOptions(%spec) => bool

Shortcut for:

 GetOptionsFromArray(\@ARGV, %spec)

=head2 GetOptionsFromArray(\@ary, %spec) => bool

Get (and strip) options from C<@ary>. Return true on success or false on failure
(unknown option, etc).


=head1 SEE ALSO

L<Getopt::Long>

L<Getopt::Long::Less>

If you want I<more> features intead of less, try L<Getopt::Long::More>.

=cut
