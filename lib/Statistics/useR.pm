package Statistics::useR;

use 5.010001;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw() ],
		     'runR' => [qw(init_R end_R eval_R)]
    );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = ( @{ $EXPORT_TAGS{'runR'} } );

our $VERSION = '0.01';

require XSLoader;
XSLoader::load('Statistics::useR', $VERSION);

package Statistics::RData;

use strict;
use warnings;

sub new {
	shift;
	my $input = {};
	%{$input} = @_;
    my %type = ('int',[],'real',[],'str',[]);;
    my $keyno = keys(%{$input->{'data'}});

#check data type    
    while(my ($key, $val) = each %{$input->{'data'}}) {
	my $t1;
	for (1..3) {
	    my $i = int(rand($#{$val}));
	    my $t2 = &getDataType($val->[$i]);
	    if(!defined $t1 || $t1 lt $t2) {
		$t1 = $t2;
	    }
	}
	push @{$type{$t1}}, $key;
    }

    my $rdata = setvalue($input->{'data'}, \%type, $keyno);
	insvar($rdata, $input->{'varname'});

    return $rdata;
}

sub getDataType {
    if(!defined $_) {
	return 'str';
	}
    if($_ =~ /^\-?(?:[1-9]\d*\.\d+|0\.\d+)$/) {
	return 'real';
    }
    elsif($_ =~ /^-?[1-9]\d*$/) {
	return 'int';
    }
    else {
	return 'str';
    }
}

1;
__END__

=head1 NAME

Statistics::useR - Embed R to let Perl speak statistics. 

=head1 SYNOPSIS

  use Statistics::useR;

=head1 DESCRIPTION

The Statistics::useR is intended to integrate statistical power of R and comprehensive power of Perl. It defines a set of methods that make it possible to communicate easily between Perl and R.

Please keep in mind that Statistics::useR is currently just a layer of glue. User is assumed to know basic ideas of R.

Statistics::useR can start an embedded R and evaluate R commands. It also offers an object-based interface to R data. The R data can be output into Perl (integer vector, real number vector and string vector, also lists containing such vectors) as a hash structure. The hash contains array refs to data. Keys in hash are names of list in R. For vector data, though, hash with only value will be put back into Perl. It can also introduce a hash into R list data. In one word, useR exchanges data with R through Perl-hash/R-list. Please see details below.

=head2 EXPORT

Statistics::useR exports three methods to run embedded R.

=head1 USAGE

	use Statistics::useR;
	use Data::Dumper;

	my $status = init_R; # init_R returns true if no error occurs. By default, R is inited with '--quiet' and '--vanilla'.
	my $cmd = 'l<-list(a=1:3,b=2:6,c=list(d=c(2,3.4,5.6),e=c(23.3,43.445),f=c(98.8,42.1,"df")));l';
	my $list = eval_R($cmd);
	print $list->gettype(), "\n"; #get data type of cmd result
	print $list->getlen(), "\n"; #get number of members of cmd result
	print join "\t", @{$list->getnames()}; print "\n"; #get names(like 'names' in R)
	print Dumper($list->getvalue()), "\n"; #Dumped value will help understand how value is returned to Perl.

	my $data = {'val',[1,4,3],'attr',['hello', 'world','universe']};
	my $rvar = Statistics::RData->new('data'=>$data, 'varname'=>'test'); #Set a new R data named 'test' in R.
	$cmd='test2<-test;test2$val <- test2$val+5;pdf("pic.pdf");plot(test2$val);dev.off();test2';
	my $res=eval_R($cmd);
	print Dumper($res->getvalue()), "\n";

	end_R(); #end R session.

=head1 METHODS

=over

=item C<init_R>

Init R with default arguments - 'quiet' and 'vanilla'. 

=item C<end_R>

End R session.

=item C<eval_R>

Evaluate R expression and return an object of RData.

=item C<new>

Create a new R list - 'data' with name 'varname'. Return an object of RData. 'data' is a hash ref, which points to hash containing array refs. Created R list takes hash keys as names, and takes every array content as component data. Refer to usage.

=item C<gettype>

Get R data type.

=item C<getlen>

Get number of members in R data.

=item C<getnames>

Just like 'names' in R.

=item C<getvalue>

Return a hash ref which contains array refs and takes names of R list components as hash keys. The arrays map to components of R list.

=back

=head1 SEE ALSO

R project website: http://www.r-project.org
    
=head1 AUTHOR

Xin Zheng, E<lt>xinzheng@cpan.orgE<gt>
Any feedback will be greatly appreciated.    

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2010 by Xin Zheng

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.10.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
