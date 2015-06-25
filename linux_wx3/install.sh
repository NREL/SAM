echo ""
echo "NREL System Advisor Model $SAMVER"
echo "Linux x86-64 Self-Extracting Installer"
echo ""

OPTDIR="/opt"
if [ ! -w $OPTDIR ] ; then
		INSTALLDIR="$HOME/SAM/$SAMVER"
else
		INSTALLDIR="/opt/SAM/$SAMVER"
fi

echo -n "Enter installation folder [enter for default: $INSTALLDIR] "
read folder
#echo "entered: $folder"
if [ -n "$folder" ] ; then
	INSTALLDIR="$folder"
fi

if [ -d "$INSTALLDIR" ] ; then
	echo -n "$INSTALLDIR already exists, overwrite SAM installation? (yes/no) "
	read yn
	YES="yes"
	if [ $yn != $YES ] ; then
			exit -1
	fi
fi

rm -rf $INSTALLDIR
mkdir -p $INSTALLDIR

if [ ! -w "$INSTALLDIR" ] ; then
		echo "No write permissions to $INSTALLDIR.  please run as root"
		exit -1
fi

#echo "installing to: $INSTALLDIR"
ARCHIVE=`awk '/^__ARCHIVE_BELOW__/ {print NR+1; exit 0; }' $0`

echo ""
echo "decompressing archive..."
CDIR=`pwd`
TMPDIR=`mktemp -d /tmp/selfextract.XXXXXX`
CSCRIPT="$CDIR/$0"
#echo "installer path: $CSCRIPT"
#echo "tail -n+$ARCHIVE $CSCRIPT | tar xz -C \"$TMPDIR\""
tail -n+$ARCHIVE $CSCRIPT | tar xz -C "$TMPDIR"

echo "copying files..."
cp -R $TMPDIR/sam.deploy/* $INSTALLDIR/

echo "removing temporary files..."
cd $INSTALLDIR
rm -rf $TMPDIR


echo "writing startup script..."
text="#!/bin/sh
cd $INSTALLDIR
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:$INSTALLDIR/linux_64
exec $INSTALLDIR/linux_64/sam.bin
"
echo "$text" > $INSTALLDIR/SAM
chmod a+x $INSTALLDIR/SAM

echo ""
echo "to uninstall, run 'rm -rf $INSTALLDIR'"
echo ""

USRBIN="/usr/bin"
if [ -w $USRBIN ] ; then
		rm -rf /usr/bin/SAM
		ln -vs $INSTALLDIR/SAM /usr/bin/SAM
		echo "SAM installed to $INSTALLDIR.  type 'SAM' to start"
else
		echo "SAM installed to $INSTALLDIR. type './SAM' from this folder to start"
fi

cd $CDIR

exit 0

__ARCHIVE_BELOW__
