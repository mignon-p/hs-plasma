#!/usr/bin/perl -w

use strict;
use FindBin;
use Getopt::Long;
use File::Path qw(make_path);

my $verbose = 0;

sub mysystem {
    my @cmd = @_;

    if ($verbose) {
        my $sep  = "";
        my $echo = "";

        foreach my $arg (@cmd) {
            $echo .= $sep;

            if ($arg =~ /\s/) {
                $echo .= "'$arg'";
            } else {
                $echo .= $arg;
            }

            $sep = " ";
        }

        print STDERR "$echo\n";
    }

    if (system (@cmd) != 0) {
        if ($? == -1) {
            die "*** fatal: $!\n";
        } elsif ($? & 127) {
            die (sprintf ("*** fatal: signal %d\n", $? & 127));
        } else {
            die (sprintf ("*** fatal: exit code %d\n", $? >> 8));
        }
    }
}

sub usage {
    print STDERR "Usage: install-man.pl [--verbose] [--prefix=PREFIX]\n";
    exit 1;
}

my $topDir    = $FindBin::Bin;
my $dataDir   = "$topDir/data";

my $shareDir  = $ENV{'XDG_DATA_HOME'};
my $homeDir   = $ENV{'HOME'};

$homeDir  = "."                     unless (defined $homeDir);
$shareDir = "$homeDir/.local/share" unless (defined $shareDir);

my $prefix = undef;

GetOptions ("prefix|p=s" => \$prefix,
            "verbose|v"  => \$verbose) or usage();
usage() if (scalar (@ARGV) > 0);

$shareDir = "$prefix/share" if (defined $prefix);

my $man1dir   = "$shareDir/man/man1";
my $srcFile   = "$dataDir/slawcat.pod";
my $dstFile   = "$man1dir/slawcat.1";
my $cabalFile = "$topDir/hs-plasma.cabal";

my $pkg  = "unknown";
my $vers = "unknown";

open F, "<", $cabalFile or die;
while (<F>) {
    chomp;
    if (/^name:\s*(\S+)/) {
        $pkg = $1;
    } elsif (/^version:\s*(\S+)/) {
        $vers = $1;
    }
}
close F;

my $release = "$pkg $vers";

my %podArgs = (
    "center"  => "User Commands",
    "release" => $release,
    );

open F, "<", $srcFile or die;
while (<F>) {
    chomp;
    last if (/^=/);
    $podArgs{$1} = $2 if (/^([a-z]+):\s*(.*)$/);
}
close F;

make_path ($man1dir, { 'verbose' => 1 });

my @cmd = ("pod2man");

foreach my $key (sort keys %podArgs) {
    my $val = $podArgs{$key};
    push @cmd, "--$key=$val";
}

push @cmd, ("--", $srcFile, $dstFile);

mysystem (@cmd);

print STDERR "Installed $dstFile\n";
