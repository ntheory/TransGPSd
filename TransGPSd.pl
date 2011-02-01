#!/usr/bin/perl -w

# TransGPSd
# by ntheory
#
# June 24th, 2002 - Started development.
#
# This program is a GPS server (similar to gpsd) that connects to a GPS
# via STDIN (serial support possibly in the future) and translates the
# raw NMEA-0183 sentences to a usable format.
#
# The external API is via SOAP.  It sends the client a structure containing 
# all the GPS data it has (and whether or not it believes this data is valid).
# There are also SOAP based functions in the object to perform data conversion 
# (knots to MPH, DMS to decimal degress, etc).
# 
# April 3rd, 2003 - Version 0.1.
#                   Updated notes, created README, removed some unused code.
#                   Posted to Sourceforge.

# -- <BEHOLD> THE GPL! --
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# -- </BEHOLD> --

use SOAP::Transport::HTTP;
use Data::Dumper;
use IO::Pipe;
use IO::Handle;
use POSIX;

# This is the total number of GPS satellites in the sky
$SatelliteCount = 32;

# Initialize a data structure to hold the temporary list of visible satellites
$VisibleSats = InitSatelliteData ();

# The port that the SOAP server runs on
$ServerPort = 8001;

# Make the pipes global so the SOAP stuff can access them
my $GPSCommandPipe;
my $GPSDataPipe;

# Ok, we're going to fork here.  We'll have one process get the NMEA info and
# another process being the SOAP server.  When the SOAP server needs data it
# will communicate with the NMEA parser via pipes.

# Make sure Data::Dumper outputs everything on a single line
$Data::Dumper::Indent = 0;

# Setup the pipes
$GPSCommandPipe = new IO::Pipe;
$GPSDataPipe    = new IO::Pipe;

if ($PID = fork ()) {
  # Parent
  $GPSDataPipe->writer ();
  $GPSCommandPipe->reader ();

  # Make sure we don't deadlock waiting for unflushed data
  $GPSDataPipe->autoflush (1);

  # Do all the fun GPS stuff
  HandleGPS ($GPSDataPipe, $GPSCommandPipe, GPSSOCKET);
}
elsif (defined $PID) {
  # We're the child process.  We'll be the SOAP server.

  # Get the pipes ready.
  $GPSCommandPipe->writer ();
  $GPSDataPipe->reader ();

  # Make sure we don't deadlock waiting for unflushed data.
  $GPSCommandPipe->autoflush (1);

  my $daemon = SOAP::Transport::HTTP::Daemon
    -> new (LocalPort => $ServerPort)
    -> dispatch_to ('GPS')
  ;

  print "Contact to SOAP server at ", $daemon->url, "\n";
  $daemon->handle;
}

exit;

sub HandleGPS {
  # In this subroutine we connect to the remote server, grab NMEA sentences,
  # identify them, parse them, update the data structure, and dump the data
  # structure to the SOAP thread when necessary

  # Get the pipes from the caller
  $GPSDataPipe = $_ [0];
  $GPSCommandPipe = $_ [1];

  # Make the command pipe non-blocking so we can easily check to see if there
  # is a request waiting
  $GPSCommandPipe->blocking (0);

  # Setup the GPS data structure
  SetupGPSDataStructure (\$GPSDataStructure);

  # There used to be special code for NetGPSd (my Visor GPS
  # server project) but irreconcilable differences between me and Palm OS
  # have since killed the project.

  # Setup handle for STDIN.
  $STDINHandle = new IO::Handle;

  if (!($STDINHandle->fdopen (fileno (STDIN),"r"))) {
    print "Couldn't open STDIN.\n";
    exit ();
  }
  else {
    $STDINHandle->autoflush (1);
  }

  while (1) {
    $NMEASentence = $STDINHandle->getline;

    # Pass a reference to the GPS data structure to the IdentifyAndParse
    # routine so we can update it elsewhere (keep the code clean)
    IdentifyAndParse ($NMEASentence, \$GPSDataStructure);

    if (defined ($GPSCommandPipe->getline ())) {
      # There is a "command" waiting.  Dump the GPS data structure to the
      # SOAP server.
      $DumpedGPSDataStructure = Dumper ($GPSDataStructure);
      print $GPSDataPipe "$DumpedGPSDataStructure\n";

      print "Parent received command.  Dumped $DumpedGPSDataStructure\n";
    }
  }
}

sub IdentifyAndParse {
  # Get the sentence
  $Sentence = shift;

  # Get the reference to the GPS data structure
  $RefGPSDataStructure = shift;

  my %Parsers = (
    "GPGSA" => \&ParseGPGSA,
    "GPGSV" => \&ParseGPGSV,
    "GPRMC" => \&ParseGPRMC,
    "GPGGA" => \&ParseGPGGA,
  );

  if (IsValidNMEA ($Sentence)) {
    # The sentence has a valid checksum, we'll give it a shot

    # Extract the talker ID and the type of NMEA sentence
    $Sentence =~ m/^\$(..)(...)/;

    $TalkerID = $1;
    $SentenceType = $2;

    $SpecificSentenceType = "$TalkerID$SentenceType";

    # See if we have a handler for this sentence type
    if ($Parsers {$SpecificSentenceType}) {
      # We're in luck, send it on its way
      $Parsers {$SpecificSentenceType}->($Sentence, $RefGPSDataStructure);
    } else {
      # No parser for this sentence type (write one!)
      print "No parser for the $SpecificSentenceType sentence coming from $TalkerID\n";
    } 
  }
}

sub ExtractSentenceData {
  $Sentence = shift;

  # Chop off the checksum
  @Data = split (/\*/, $Sentence);

  # Now we look just at the satellite data
  @Data = split (/,/, $Data [0]);

  return @Data;
}

sub ParseGPGSA {
  # Get the sentence
  $Sentence = shift;

  # Get the reference to the GPS data structure and dereference it
  $RefGPSDataStructure = shift;
  $GPSDataStructure = ${$RefGPSDataStructure};

  # Extract the raw sentence data
  @Data = ExtractSentenceData ($Sentence);

  # Grab the satellite data
  $GPSDataStructure->{FixMode1} = $Data [1];
  $GPSDataStructure->{FixMode2} = POSIX::strtol ($Data [2]);

  # Set the number base.  This is for strtol conversions.  If we don't set the base manually
  # the numbers may incorrectly be converted from octal to decimal when they have a leading
  # zero.
  $Base = 10;

  $InitialPosition = 3;
  $Position        = $InitialPosition;
  $CurrentChannel  = $Position - $InitialPosition;

  # While there is still more data fill the satellite structure
  while (defined ($Data [$Position]) && ($CurrentChannel < 12)) {
    $Channels->{Data} [$CurrentSatellite]->{PRN} = POSIX::strtol ($Data [$Position], $Base);

    $Position++;
    $CurrentChannel = $Position - $InitialPosition;
  }

  $GPSDataStructure->{PDOP} = POSIX::strtod ($Data [15]);
  $GPSDataStructure->{HDOP} = POSIX::strtod ($Data [16]);
  $GPSDataStructure->{VDOP} = POSIX::strtod ($Data [17]);

  $GPSDataStructure->{Channels} = DeepCopy ($Channels);
}

sub ParseGPGSV {
  # Get the sentence
  $Sentence = shift;

  # Get the reference to the GPS data structure and dereference it
  $RefGPSDataStructure = shift;
  $GPSDataStructure = ${$RefGPSDataStructure};

  # Extract the raw sentence data
  @Data = ExtractSentenceData ($Sentence);

  # Grab the satellite data
  $SentenceCount  = POSIX::strtol ($Data [1]);
  $SentenceNumber = POSIX::strtol ($Data [2]);

  # Set the number base.  This is for strtol conversions.  If we don't set the base manually
  # the numbers may incorrectly be converted from octal to decimal when they have a leading
  # zero.
  $Base = 10;

  if ($SentenceNumber == 1) {
    # If it's the first satellite we'll clear out all the satellite data
    $VisibleSats = InitSatelliteData ();
  }

  # Position us into the array where the satellite data starts
  $Position = 4;

  # While there is still more data fill the satellite structure
  while (defined ($Data [$Position])) {
    $CurrentSatellite = $VisibleSats->{Count}++;

    # The last signal strength will end up being undefined if it is zero, make sure that doesn't happen.
    # This is because the split function in ExtractSentenceData doesn't define empty values if everything
    # after the empty value is also empty.
    if (!defined ($Data [$Position + 3])) {
      $Data [$Position + 3] = 0;
    }

    $VisibleSats->{Data} [$CurrentSatellite]->{PRN}            = POSIX::strtol ($Data [$Position], $Base);
    $VisibleSats->{Data} [$CurrentSatellite]->{Altitude}       = POSIX::strtol ($Data [$Position + 1], $Base);
    $VisibleSats->{Data} [$CurrentSatellite]->{Azimuth}        = POSIX::strtol ($Data [$Position + 2], $Base);
    $VisibleSats->{Data} [$CurrentSatellite]->{SignalStrength} = POSIX::strtol ($Data [$Position + 3], $Base);

    $Position += 4;
  }

  if ($SentenceCount == $SentenceNumber) {
    # This is the last GSV sentence.  Update all the satellite info at once.
    # Do this the right way with a deep copy to avoid the data structures
    # from overwriting each other.
    $GPSDataStructure->{Satellites} = DeepCopy ($VisibleSats);
  }
}

sub ParseGPGGA {
  # Get the sentence
  $Sentence = shift;

  # Get the reference to the GPS data structure and dereference it
  $RefGPSDataStructure = shift;
  $GPSDataStructure = ${$RefGPSDataStructure};

  # Extract the raw sentence data
  @Data = ExtractSentenceData ($Sentence);

  # Get the basic data ($Data [0] is the NMEA talker ID and sentence type)

  # We need these values for every piece of data, let's store them here to
  # make our assignment statements shorter.
  $UTCFixTime = $Data [1];
  $Quality    = $Data [6];
  $Valid      = ($Quality != 0);

  $GPSDataStructure->{UTCFixTime}            = $UTCFixTime;

  $GPSDataStructure->{Latitude}              = $Data [2];
  $GPSDataStructure->{LatitudeDirection}     = $Data [3];
  $GPSDataStructure->{Longitude}             = $Data [4];
  $GPSDataStructure->{LongitudeDirection}    = $Data [5];
  $GPSDataStructure->{SatellitesTracked}     = $Data [7];
  $GPSDataStructure->{MSLAltitude}           = $Data [8];
  $GPSDataStructure->{MSLAltitudeUnits}      = $Data [9];
  $GPSDataStructure->{MSLAltitudeWGS84}      = $Data [10];
  $GPSDataStructure->{MSLAltitudeWGS84Units} = $Data [11];
  $GPSDataStructure->{SecondsSinceDGPS}      = $Data [12];
  $GPSDataStructure->{DGPSID}                = $Data [13];

  # Refine some of the data

  # Set the number base.  This is for strtol conversions.  If we don't set the base manually
  # the numbers may incorrectly be converted from octal to decimal when they have a leading
  # zero.
  $Base = 10;

  # Make the UTC fix time look pretty.  We can get hundredths of a second if we have the
  # satellites locked.  Might want to add this later.
  $GPSDataStructure->{UTCFixTime} =~ m/(..)(..)(..)/;
  $GPSDataStructure->{UTCFixTime} = "$1:$2:$3";
  $UTCFixTime = $GPSDataStructure->{UTCFixTime};

  # XXX - I am unsure if latitudes below 10 degrees have a leading zero.
  # If they do not that WILL cause a problem.
  $GPSDataStructure->{Latitude} =~ m/(..)(..)\.(....)/;
  ($GPSDataStructure->{LatitudeDegrees}, $Unparsed) = POSIX::strtol ($1, $Base);
  ($GPSDataStructure->{LatitudeMinutes}, $Unparsed) = POSIX::strtod ("$2.$3");

  # Convert the latitude to decimal
  $GPSDataStructure->{LatitudeDecimal} = ConvertDMmToDecimal ($GPSDataStructure->{LatitudeDegrees},
                                                              $GPSDataStructure->{LatitudeMinutes},
							      $GPSDataStructure->{LatitudeDirection});

  # XXX - I am unsure if longitudes below 10 degrees have two leading zeroes.
  # If they do not that WILL cause a problem.  This is unlikely though because
  # longitudes below 100 contain the first leading zero.
  $GPSDataStructure->{Longitude} =~ m/(...)(..)\.(....)/;
  ($GPSDataStructure->{LongitudeDegrees}, $Unparsed) = POSIX::strtol ($1, $Base);
  ($GPSDataStructure->{LongitudeMinutes}, $Unparsed) = POSIX::strtod ("$2.$3");

  # Convert the longitude to decimal
  $GPSDataStructure->{LongitudeDecimal} = ConvertDMmToDecimal ($GPSDataStructure->{LongitudeDegrees},
                                                               $GPSDataStructure->{LongitudeMinutes},
						               $GPSDataStructure->{LongitudeDirection});

  # Convert horizontal dilution to a double
  ($GPSDataStructure->{HorizontalDilution}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{HorizontalDilution});

  # Convert MSL altitude to a double
  ($GPSDataStructure->{MSLAltitude}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{MSLAltitude});

  # Convert MSL altitude (WGS84 ellipsoid) to a double
  ($GPSDataStructure->{MSLAltitudeWGS84}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{MSLAltitudeWGS84});

  # Convert time in seconds since last DGPS update to a number
  ($GPSDataStructure->{SecondsSinceDGPS}, $Unparsed) = POSIX::strtol ($GPSDataStructure->{SecondsSinceDGPS}, $Base);

  # Mark all the data with the time it was acquired, its validity, its quality, and the actual data.
  $GPSDataStructure->{Latitude}              = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{Latitude});
  $GPSDataStructure->{LatitudeDirection}     = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LatitudeDirection});
  $GPSDataStructure->{LatitudeDecimal}       = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LatitudeDecimal});
  $GPSDataStructure->{Longitude}             = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{Longitude});
  $GPSDataStructure->{LongitudeDirection}    = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LongitudeDirection});
  $GPSDataStructure->{LongitudeDecimal}      = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LongitudeDecimal});
  $GPSDataStructure->{SatellitesTracked}     = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{SatellitesTracked});
  $GPSDataStructure->{MSLAltitude}           = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MSLAltitude});
  $GPSDataStructure->{MSLAltitudeUnits}      = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MSLAltitudeUnits});
  $GPSDataStructure->{MSLAltitudeWGS84}      = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MSLAltitudeWGS84});
  $GPSDataStructure->{MSLAltitudeWGS84Units} = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MSLAltitudeWGS84Units});
  $GPSDataStructure->{SecondsSinceDGPS}      = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{SecondsSinceDGPS});
  $GPSDataStructure->{DGPSID}                = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{DGPSID});
}

sub ParseGPRMC {
  # Get the sentence
  $Sentence = shift;

  # Get the reference to the GPS data structure and dereference it
  $RefGPSDataStructure = shift;
  $GPSDataStructure = ${$RefGPSDataStructure};

  # Extract the raw sentence data
  @Data = ExtractSentenceData ($Sentence);

  # Get the basic data ($Data [0] is the NMEA talker ID and sentence type)

  # We need these values for every piece of data, let's store them here to
  # make our assignment statements shorter.
  $UTCFixTime = $Data [1];
  $Valid      = $Data [2] eq "A";
  $Quality    = $Valid;

  $GPSDataStructure->{UTCFixTime}            = $UTCFixTime;
  $GPSDataStructure->{Latitude}              = $Data [3];
  $GPSDataStructure->{LatitudeDirection}     = $Data [4];
  $GPSDataStructure->{Longitude}             = $Data [5];
  $GPSDataStructure->{LongitudeDirection}    = $Data [6];
  $GPSDataStructure->{SpeedKnots}            = $Data [7];
  $GPSDataStructure->{Course}                = $Data [8];
  $GPSDataStructure->{UTCFixDate}            = $Data [9];
  $GPSDataStructure->{MagVariation}          = $Data [10];
  $GPSDataStructure->{MagVariationDirection} = $Data [11];

  # Refine some of the data

  # Set the number base.  This is for strtol conversions.  If we don't set the base manually
  # the numbers may incorrectly be converted from octal to decimal when they have a leading
  # zero.
  $Base = 10;

  # Make the UTC fix time look pretty.  We can get hundredths of a second if we have the
  # satellites locked.  Might want to add this later.
  $GPSDataStructure->{UTCFixTime} =~ m/(..)(..)(..)/;
  $GPSDataStructure->{UTCFixTime} = "$1:$2:$3";
  $UTCFixTime = $GPSDataStructure->{UTCFixTime};

  # Make the UTC fix date look pretty (and convert it to the US format)
  $GPSDataStructure->{UTCFixDate} =~ m/(..)(..)(..)/;
  $GPSDataStructure->{UTCFixDate} = "$2/$1/$3";

  # XXX - I am unsure if latitudes below 10 degrees have a leading zero.
  # If they do not that WILL cause a problem.
  $GPSDataStructure->{Latitude} =~ m/(..)(..)\.(....)/;
  ($GPSDataStructure->{LatitudeDegrees}, $Unparsed) = POSIX::strtol ($1, $Base);
  ($GPSDataStructure->{LatitudeMinutes}, $Unparsed) = POSIX::strtod ("$2.$3");

  # Convert the latitude to decimal
  $GPSDataStructure->{LatitudeDecimal} = ConvertDMmToDecimal ($GPSDataStructure->{LatitudeDegrees},
                                                              $GPSDataStructure->{LatitudeMinutes},
							      $GPSDataStructure->{LatitudeDirection});

  # XXX - I am unsure if longitudes below 10 degrees have two leading zeroes.
  # If they do not that WILL cause a problem.  This is unlikely though because
  # longitudes below 100 contain the first leading zero.
  $GPSDataStructure->{Longitude} =~ m/(...)(..)\.(....)/;
  ($GPSDataStructure->{LongitudeDegrees}, $Unparsed) = POSIX::strtol ($1, $Base);
  ($GPSDataStructure->{LongitudeMinutes}, $Unparsed) = POSIX::strtod ("$2.$3");

  # Convert the longitude to decimal
  $GPSDataStructure->{LongitudeDecimal} = ConvertDMmToDecimal ($GPSDataStructure->{LongitudeDegrees},
                                                               $GPSDataStructure->{LongitudeMinutes},
						               $GPSDataStructure->{LongitudeDirection});

  # Convert speed from a string to a double
  ($GPSDataStructure->{SpeedKnots}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{SpeedKnots});

  # Convert course to a double
  ($GPSDataStructure->{Course}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{Course});

  # Convert magnetic variation to a double
  if (!defined ($GPSDataStructure->{MagVariation})) {
    $GPSDataStructure->{MagVariation} = 0;
  }

  ($GPSDataStructure->{MagVariation}, $Unparsed) = POSIX::strtod ($GPSDataStructure->{MagVariation});

  # Mark all the data with the time it was acquired, its validity, its quality, and the actual data.
  $GPSDataStructure->{UTCFixTime}            = $UTCFixTime;
  $GPSDataStructure->{Latitude}              = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{Latitude});
  $GPSDataStructure->{LatitudeDirection}     = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LatitudeDirection});
  $GPSDataStructure->{LatitudeDecimal}       = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LatitudeDecimal});
  $GPSDataStructure->{Longitude}             = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{Longitude});
  $GPSDataStructure->{LongitudeDirection}    = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LongitudeDirection});
  $GPSDataStructure->{LongitudeDecimal}      = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{LongitudeDecimal});
  $GPSDataStructure->{SpeedKnots}            = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{SpeedKnots});
  $GPSDataStructure->{Course}                = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{Course});
  $GPSDataStructure->{UTCFixDate}            = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{UTCFixDate});
  $GPSDataStructure->{MagVariation}          = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MagVariation});
  $GPSDataStructure->{MagVariationDirection} = PackageDataElement ($UTCFixTime, $Valid, $Quality, $GPSDataStructure->{MagVariationDirection});
}

sub PackageDataElement {
  # We're going to put the data into a structure and package it with relevant information
  # such as when it was acquired and how much the acquisition can be trusted
  $UTCFixTime = shift;
  $Valid = shift;
  $Quality = shift;
  $Data = shift;
  
  $ReturnPackage = {
    UTCFixTime => $UTCFixTime,
    Valid      => $Valid,
    Quality    => $Quality,
    Data       => $Data
  };

  return $ReturnPackage;
}

sub IsValidNMEA {
  # Get the sentence
  $Sentence = shift;

  # Get the checksum out of the sentence
  @Data = split (/\*/, $Sentence);
  $ReceivedChecksum = $Data [1];

  $CalculatedChecksum = 0;

  for ($ReceivedChecksumLoop = 1; $ReceivedChecksumLoop < length ($Data [0]); $ReceivedChecksumLoop++) {
    $CurrentCharacter = substr ($Data [0], $ReceivedChecksumLoop, 1);
    $CalculatedChecksum = $CalculatedChecksum ^ ord ($CurrentCharacter);
  }

  # Remove the trailing \n (or \r\n) on the NMEA sentence checksum
  while (length ($ReceivedChecksum) > 2) {
    chop $ReceivedChecksum;
  }

  $ReceivedChecksum = hex ($ReceivedChecksum);

  if ($CalculatedChecksum eq $ReceivedChecksum) {
    return 1;
  }
  else {
    return 0;
  }
}

sub SetupGPSDataStructure {
  # Get the reference to the GPS data structure and dereference it
  $RefGPSDataStructure = shift;
  $GPSDataStructure = ${$RefGPSDataStructure};

  $UTCFixTime = "00:00:00";
  $Valid      = 0;
  $Quality    = 0;

  $GPSDataStructure = {
    # These are from the GPRMC sentence
    UTCFixTime            => $UTCFixTime,
    Latitude              => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    LatitudeDirection     => PackageDataElement ($UTCFixTime, $Valid, $Quality, "N"),
    LatitudeDecimal       => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    Longitude             => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    LongitudeDirection    => PackageDataElement ($UTCFixTime, $Valid, $Quality, "W"),
    LongitudeDecimal      => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    SpeedKnots            => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    Course                => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    UTCFixDate            => PackageDataElement ($UTCFixTime, $Valid, $Quality, "00/00/00"),
    MagVariation          => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    MagVariationDirection => PackageDataElement ($UTCFixTime, $Valid, $Quality, "E"),

    # These are added in the GPGGA sentence
    SatellitesTracked     => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    HorizontalDilution    => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    MSLAltitude           => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    MSLAltitudeUnits      => PackageDataElement ($UTCFixTime, $Valid, $Quality, "M"),
    MSLAltitudeWGS84      => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    MSLAltitudeWGS84Units => PackageDataElement ($UTCFixTime, $Valid, $Quality, "M"),
    SecondsSinceDGPS      => PackageDataElement ($UTCFixTime, $Valid, $Quality, 0),
    DGPSID                => PackageDataElement ($UTCFixTime, $Valid, $Quality, ""),

    # Clear out the satellite data
    Satellites            => InitSatelliteData (),
  };

}

sub ConvertDMmToDecimal {
  $Degrees = shift;
  $Minutes = shift;
  $Direction = shift;

  if (($Direction eq "N") || ($Direction eq "E")) {
    # A positive value
    $Decimal = $Degrees + ($Minutes / 60);
  }
  elsif (($Direction eq "S") || ($Direction eq "W")) {
    # A negative value
    $Decimal = 0 - ($Degrees + ($Minutes / 60));
  }

  return $Decimal;
}

sub InitSatelliteData {
  # Clear out all of the satellite data
  for ($Loop = 0; $Loop < $SatelliteCount; $Loop++) {
    $Satellites->{Count} = 0;

    $Satellites->{Data} [$Loop] = {
      PRN            => -1,
      Altitude       => -1,
      Azimuth        => -1,
      SignalStrength => -1
    };
  }

  return $Satellites;
}

sub DeepCopy {
  my $this = shift;

  if (not ref $this) {
    $this;
  } elsif (ref $this eq "ARRAY") {
    [map DeepCopy($_), @$this];
  } elsif (ref $this eq "HASH") {
    +{map { $_ => DeepCopy($this->{$_}) } keys %$this};
  } else { die "what type is $_?" }
}

package GPS;

sub Get {
  # Someone contacted the SOAP server.  Send a request to the GPS data server
  # through the GPSCommandPipe.
  print $GPSCommandPipe "REQUEST\n";

  # Give the client that contacted the SOAP server the data structure that
  # was dumped through the GPSDataPipe.  We will store it locally and then
  # send it off.
  $StructureReceived = eval ($GPSDataPipe->getline ());

  print "Valid: " . $StructureReceived->{LatitudeDecimal}->{Valid} . "\n";
  print "All: " . $StructureReceived . "\n";

  return $StructureReceived;
}
