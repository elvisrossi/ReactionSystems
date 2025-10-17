#!/usr/bin/perl
use warnings;
use strict;

my %structure = (
    Environment => (),
    InitialEntities => (),
    Context => (),
    Reactions => (),
    );

open FILE, $ARGV[0] or die $!;
my @data = <FILE>;
my $data = "@data";

$data =~ /myentities\(\[([^\]]*)\]\)/s;
$structure{InitialEntities} = $1;

$data =~ /myenvironment\(\"\[([^\]]*)\]\"\)/s;
$structure{Environment} = $1;

$data =~ /mycontext\(\"\[([^\]]*)\]\"\)/s;
$structure{Context} = $1;

my $line;
for $line (@data) {
    $_ = $line;
    if (/react.*\]\),?$/) {
        s/react\(\[(.*?)\],\[(.*?)\],\[(.*?)\]\),?/[{$1}, {$2}, {$3}]/;
        chomp $_;
        $structure{Reactions} .= "$_;\n";
    }
}


print ("Environment: [", $structure{Environment}, "]\n");
print ("Initial Entities: {", $structure{InitialEntities} ,"}\n");
print ("Context: [", $structure{Context}, "]\n");
print ("Reactions: (\n", $structure{Reactions}, ")\n");
