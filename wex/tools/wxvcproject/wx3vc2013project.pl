#!/usr/bin/perl
use File::Copy;
use File::Path qw(make_path remove_tree);

chomp( $guid = uc(`GuidGenConsole.exe`) );

print "GUID=$guid\n";

print "Enter project name: ";
chomp($prjname = <STDIN>);

make_path($prjname, {
      verbose => 1,
      mode => 0711,
  } );

sub convert_file {
  my($from, $to);       # new, private variables for this block
  ($from, $to) = @_;    # give names to the parameters
	open(FROM, $from) or die "could not read $from\n";
	open(TO, ">$prjname/$to") or die "could not write $to\n";
	while( $line = <FROM> )
	{
		$line =~ s/template/$prjname/g;
		$line =~ s/{guid}/{$guid}/g;
		print TO $line;
	}

	print "Wrote $prjname/$to\n";

	close FROM;
	close TO;
}

convert_file( "template.vcxproj", "$prjname.vcxproj" );
convert_file( "Info-template.plist", "Info-$prjname.plist" );
convert_file( "Makefile-template", "Makefile-$prjname" );
convert_file( "template.cpp", "$prjname.cpp");
copy("template.rc", "$prjname/$prjname.rc");
copy("icons/monitor.ico", "$prjname/app.ico");
copy("template.icns", "$prjname/$prjname.icns");
