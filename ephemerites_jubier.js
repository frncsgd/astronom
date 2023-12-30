<!--
// Solar, lunar and planets ephemerides (Xavier Jubier: http://xjubier.free.fr/)
//
// Modifications:
// 2020-05-26   Xavier Jubier   Correction on the Equation of Time (wrong 24-hour offset)
// 2004-06-10   Xavier Jubier   Added language parameter to initEphemerides()
//

var myLanguage = "en";
var gVisibility = "";
var gWhen = "";
var PI = Math.PI;
var PIx2 = 2.0 * Math.PI;
var D2R = Math.PI / 180.0;
var R2D = 180.0 / Math.PI;
var H2R = Math.PI / 12.0;
var R2H = 12.0 / Math.PI;
var INVERSE_SIDEREAL_DAY = 1.002737909350795;
var K1 = 15.0 * D2R * INVERSE_SIDEREAL_DAY;
var AU = 149597870.691; // 1 AU in kilometers
var ER = 6378.137; // Earth radius in kilometers (semi-major axis)
var MR = 3474.2; // Moon mean radius in kilometers
var SUNPARALLAX = 8.794142;
var day, month, year, hour, minute, second;
var flattening = 1.0 / 298.257223563;
var Moonrise = false;
var Moonset = false;
var Rise_time = [0, 0];
var Set_time  = [0, 0];
var Rise_az = 0.0;
var Set_az  = 0.0;
var Sky = [0.0, 0.0, 0.0];
var RAn = [0.0, 0.0, 0.0];
var Decl = [0.0, 0.0, 0.0];
var VHz = [0.0, 0.0, 0.0];
var jd, GMST, nutationRA_corr;
var latObs, lonObs, elvObs;
var RADecSun = new Object();
var RADecMoon = new Object();
var RADecMerc = new Object();
var RADecVen = new Object();
var RADecMars = new Object();
var RADecJup = new Object();
var RADecSat = new Object();
var RADecNept = new Object();
var RADecUran = new Object();
var RADecPluto = new Object();

var MonthsFr = new Array( "janvier", "f\xE9vrier", "mars", "avril", "mai", "juin", "juillet", "ao\xFBt", "septembre", "octobre", "novembre", "d\xE9cembre" );
var MonthsEn = new Array( "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" );


function initEphemerides( language )
{
  if ( language != undefined )
    myLanguage = language;

  date(language);

  document.getElementById("day").focus();
  document.getElementById("day").select();
}

// Start updating the data every two seconds
function update( language )
{
  if (language == "fr")
    theTime = setTimeout("update('fr')", 2000);   // Every 2 seconds
  else
    theTime = setTimeout("update('en')", 2000);   // Every 2 seconds
  date(language);
}

function stop( )
{
  clearTimeout(theTime);
}

function date( language )
{
  var today = new Date();
  day = today.getUTCDate();
  month = today.getUTCMonth() + 1;
  year = today.getUTCFullYear();
  hour = today.getUTCHours();
  minute = today.getUTCMinutes();
  second = today.getUTCSeconds();
  var lhour = today.getHours();
  var lminute = today.getMinutes();
  var lsecond = today.getSeconds();
  var gmt_offset = today.getTimezoneOffset();

  document.getElementById("day").value = day;
  document.getElementById("month").value = month;
  document.getElementById("year").value = year;
  document.getElementById("hour").value = hour;
  document.getElementById("minute").value = minute;
  document.getElementById("second").value = second;
  document.getElementById("local_hour").innerHTML = zero(lhour) + "h";
  document.getElementById("local_minute").innerHTML = zero(lminute) + "m";
  document.getElementById("local_second").innerHTML = zero(lsecond) + "s";
  document.getElementById("gmt_offset").innerHTML = ((gmt_offset < 0) ? "+" : "") + (-gmt_offset / 60);

  refresh(language);
}

// Handle a location modification
function modifiedCoordinates( which, language )
{
  var ddStr;

  if ( which == 1 ) // Latitude
  {
    if ( document.getElementById("Lat_dd") )
    {
      ddStr = parseInt("0" + document.getElementById("latd").value, 10);
      ddStr += parseFloat((document.getElementById("latm").value == "") ? "0.0" : document.getElementById("latm").value.replace(/\,/, '.')) / 60.0;
      if ( ! isNaN(ddStr) )
      {
        ddStr = Math.abs(ddStr);
        if ( ddStr > 90.0 )
        {
          if (myLanguage == "fr")
            alert("Saisissez une latitude comprise entre 0 et 90 degr\xE9s, Nord ou Sud.");
          else
            alert("Please enter a latitude between 0 and 90 degrees, North or South.");
          document.getElementById("latd").focus();
          document.getElementById("latd").select();
          return false;
        }
        if ( document.getElementById("latx").selectedIndex == 1 ) // South
          ddStr = -ddStr;
        ddStr = ddStr.toFixed(5);
        if (language == "fr")
          ddStr = ddStr.replace(/\./, ',');
        document.getElementById("Lat_dd").innerHTML = "&nbsp;&mdash;&gt;&nbsp;" + ddStr + "&deg;";
      }
      else
        document.getElementById("Lat_dd").innerHTML = "";
    }
  }
  else // Longitude
  {
    if ( document.getElementById("Lon_dd") )
    {
      ddStr = parseInt("0" + document.getElementById("lond").value, 10);
      ddStr += parseFloat((document.getElementById("lonm").value == "") ? "0.0" : document.getElementById("lonm").value.replace(/\,/, '.')) / 60.0;
      if ( ! isNaN(ddStr) )
      {
        ddStr = Math.abs(ddStr);
        if ( ddStr > 180.0 )
        {
          if (myLanguage == "fr")
            alert("Saisissez une longitude comprise entre 0 et 180 degr\xE9s, Est ou Ouest.");
          else
            alert("Please enter a longitude between 0 and 180 degrees, East or West.");
          document.getElementById("lond").focus();
          document.getElementById("lond").select();
          return false;
        }
        if ( document.getElementById("lonx").selectedIndex == 1 ) // West
          ddStr = -ddStr;
        ddStr = ddStr.toFixed(5);
        if (language == "fr")
          ddStr = ddStr.replace(/\./, ',');
        document.getElementById("Lon_dd").innerHTML = "&nbsp;&mdash;&gt;&nbsp;" + ddStr + "&deg;";
      }
      else
        document.getElementById("Lon_dd").innerHTML = "";
    }
  }
}

// Clear the results
function clearresults( )
{
  return;
}

function dayOfWeek( jd, language )
{
  var dayNb = Math.round(jd) % 7;

  if (language == "fr")
  {
    switch ( dayNb )
    {
      case 0:
        weekday = "lundi";
        break;
      case 1:
        weekday = "mardi";
        break;
      case 2:
        weekday = "mercredi";
        break;
      case 3:
        weekday = "jeudi";
        break;
      case 4:
        weekday = "vendredi";
        break;
      case 5:
        weekday = "samedi";
        break;
      case 6:
        weekday = "dimanche";
        break;
    }
  }
  else
  {
    switch ( dayNb )
    {
      case 0:
        weekday = "Monday";
        break;
      case 1:
        weekday = "Tuesday";
        break;
      case 2:
        weekday = "Wednesday";
        break;
      case 3:
        weekday = "Thursday";
        break;
      case 4:
        weekday = "Friday";
        break;
      case 5:
        weekday = "Saturday";
        break;
      case 6:
        weekday = "Sunday";
        break;
    }
  }

  document.getElementById("weekday").innerHTML = weekday;
}

function visible_when_where( planet_elongation, elongation, threshold, language )
{
  if ( elongation <= threshold )
  {
    if (language == "fr")
      gVisibility = "Non observable";
    else
      gVisibility = "Not observable";
    gWhen = " ";
  }
  if ( (elongation > threshold) && (elongation <= 20) )
  {
    if (language == "fr")
      gVisibility = "Difficilement visible";
    else
      gVisibility = "Hardly visible";
    switch ( planet_elongation )
    {
      case "Ouest":
        gWhen = "peu avant le lever du Soleil";
        break;
      case "Est":
        gWhen = "au coucher du Soleil";
        break;
      case "West":
        gWhen = "just before sunrise";
        break;
      case "East":
        gWhen = "at sunset";
        break;
    }
  }
  if ( (elongation > 20) && (elongation <= 50) )
  {
    gVisibility = "Visible";
    switch ( planet_elongation )
    {
      case "Ouest":
        gWhen = "en toute fin de nuit";
        break;
      case "Est":
        gWhen = "en tout d\xE9but de soir\xE9e";
        break;
      case "West":
        gWhen = "at the end of the night";
        break;
      case "East":
        gWhen = "at the beginning of the evening";
        break;
    }
  }
  if ( (elongation > 50) && (elongation <= 120) )
  {
    gVisibility = "Visible";
    switch ( planet_elongation )
    {
      case "Ouest":
        gWhen = "en seconde partie de nuit";
        break;
      case "Est":
        gWhen = "en premi\xE8re partie de nuit";
        break;
      case "West":
        gWhen = "during the second half of the night";
        break;
      case "East":
        gWhen = "during the first half of the night";
        break;
    }
  }
  if ( (elongation > 120) && (elongation <= 140) )
  {
    gVisibility = "Visible";
    if (language == "fr" )
      gWhen = "pratiquement toute la nuit";
    else
      gWhen = "for nearly the whole night";
  }
  if ( (elongation > 140) && (elongation <= 180) )
  {
    gVisibility = "Visible";
    if (language == "fr")
      gWhen = "toute la nuit";
    else
      gWhen = "during the whole night";
  }
}

function refresh( language )
{
  var m;
  var distEarthRadii = new Array(10);

  if (language == "fr")
    window.status = "Calcul en cours...";
  else
    window.status = "Running...";

   // Observer's latitude
  var latd = parseInt(document.getElementById("latd").value, 10);
  var latm = parseFloat(document.getElementById("latm").value.replace(/\,/, '.'));
  var latx = parseInt(document.getElementById("latx").value, 10);
  if ( isNaN(latd) || (Math.abs(latd) >= 90) || isNaN(latm) || (latm < 0) || (latm >= 60) )
  {
    if (language == "fr")
      alert("Latitude de l\u2019observateur erron\xE9e!");
    else
      alert("Invalid observer latitude!");
    document.getElementById("latd").focus();
    document.getElementById("latd").select();
    return;
  }
  latObs = (latd + (latm / 60)) * D2R;
  latObs *= latx;
   // Observer's longitude (positive to the West)
  var lngd = parseInt(document.getElementById("lond").value, 10);
  var lngm = parseFloat(document.getElementById("lonm").value.replace(/\,/, '.'));
  var lngx = parseInt(document.getElementById("lonx").value, 10);
  if ( isNaN(lngd) || (Math.abs(lngd) >= 180) || isNaN(lngm) || (lngm < 0) || (lngm >= 60) )
  {
    if (language == "fr")
      alert("Longitude de l\u2019observateur erron\xE9e!");
    else
      alert("Invalid observer longitude!");
    document.getElementById("lond").focus();
    document.getElementById("lond").select();
    return;
  }
  lonObs = (lngd + (lngm / 60)) * D2R;
  lonObs *= lngx;
   // Observer's elevation
  elvObs = parseFloat(document.getElementById("elv").value.replace(/\,/, '.'));

  day = parseInt(document.getElementById("day").value, 10);
  month = parseInt(document.getElementById("month").value, 10);
  year = parseInt(document.getElementById("year").value, 10);
  hour = parseInt(document.getElementById("hour").value, 10);
  minute = parseInt(document.getElementById("minute").value, 10);
  second = parseInt(document.getElementById("second").value, 10);
  var now = new Date(year, month - 1, day, hour, minute, second);
  if ( isNaN(now.getTime()) || (month < 1) || (month > 12) || (day < 1) || (day > 31) || (hour < 0) || (hour >= 24) || (minute < 0) || (minute >= 60) || (second < 0) || (second >= 60) )
  {
    if (language == "fr")
      alert("Date et/ou heure erron\xE9es!");
    else
      alert("Invalid date and/or time!");
    document.getElementById("day").focus();
    document.getElementById("day").select();
    return;
  }

   // Julian day
  jd = julianDay(day, month, year, hour, minute, second);
  document.getElementById("julianday").innerHTML = fixedn(jd, 6);

  dayOfWeek(jd, language);

   // DeltaT
  var dT = deltaT();
  if (language == "fr")
    document.getElementById("dt").innerHTML = fixedn(dT, 2) + "&nbsp;secondes";
  else
    document.getElementById("dt").innerHTML = fixedn(dT, 2) + "&nbsp;seconds";

   // Julian ephemeris day (JDE TT)
  var jde = jd + (dT / 86400.0);
  document.getElementById("julianday2").innerHTML = fixedn(jde, 6);

   // Earth Ephemeris Time TT
  var cDate = julianDay2calendarDate(jde);
  var cTime = julianDay2hms(jde);
  document.getElementById("utday").innerHTML = cDate[2];
  document.getElementById("utmonth").innerHTML = cDate[1];
  document.getElementById("utyear").innerHTML = cDate[0];
  document.getElementById("uth").innerHTML = zero(cTime[0]);
  document.getElementById("utm").innerHTML = zero(cTime[1]);
  document.getElementById("uts").innerHTML = zero(Math.floor(cTime[2]));

   // Time in Julian centuries of 36525 days from 1899 December 31 at noon UT (IAU ref.)
  var T = (jde - 2415020.0) / 36525.0;
  var T2 = T * T;
  var T3 = T2 * T;

   // Epoch J2000: 2000 January 1 at noon UT
  var T_J2000 = (jde - 2451545.0) / 36525.0;
  var T2_J2000 = T_J2000 * T_J2000;
  var T3_J2000 = T2_J2000 * T_J2000;
  var T4_J2000 = T2_J2000 * T2_J2000;

   // Greenwich mean sidereal time at a given hour (GMST)
  GMST = GreenwichMeanSiderealTime(jd);
  showHMS(GMST, "gst");

   // Local sidereal time
  var LST1 = GMST + (-lonObs * R2H);
  if ( LST1 < 0.0 )
    LST1 += 24.0;
  else if ( LST1 > 24.0 )
    LST1 -= 24.0;
  showHMS(LST1, "lst");

  // Greenwich mean sidereal time at 00:00 UT (GMST0)
  var J0 = Math.floor(jd + 0.5) - 0.5;  // Julian day at 00:00 UT
  var T0 = (J0 - 2451545.0) / 36525.0;
  var MST0 = 100.46061837 + (T0 * (36000.770053608 + (T0 * (0.000387933 - (T0 / 38710000.0)))));
  MST0 = rev(MST0);
  var GMST0 = MST0 / 15.0; // Greenwich mean sidereal time at 00:00 UT (without the nutation correction)

  nutationRA_corr = nutation_RA(jde);

   //----
   // Sun (lower accuracy)
   //----

   // Mean longitude
  var L = rev(279.69668 + (36000.76892 * T) + (0.0003025 * T2)) * D2R;
   // Mean anomaly
  var M = (358.47583 + (35999.04975 * T) - (0.00015 * T2) - (0.0000033 * T3)) * D2R;
   // Eccentricity of Earth's orbit
  var e = 0.01675104 - (0.0000418 * T) - (0.000000126 * T2);
   // Equation of the Center
  var c = ((1.919460 - (0.004789 * T) - (0.000014 * T2)) * Math.sin(M) + (0.020094 - (0.000100 * T)) * Math.sin(2 * M) + (0.000293 * Math.sin(3 * M))) * D2R;
   // True longitude and true anomaly of the Sun (refered to the mean equinox of date)
  var true_long = L + c;
  var v = M + c;

   // Corrections to the solar longitude
  var corrA = (153.23 + (22518.7541 * T)) * D2R; // For Venus
  var corrB = (216.57 + (45037.5082 * T)) * D2R; // For Venus
  var corrC = (312.69 + (32964.3577 * T)) * D2R; // For Jupiter
  var corrD = (350.74 + (445267.1142 * T) - (0.00144 * T2)) * D2R; // For the Moon
  var corrE = (231.19 + (20.20 * T)) * D2R; // Long period term
  var corrH = (353.40 + (65928.7155 * T)) * D2R;

  var omega = (259.18 - (1934.142 * T)) * D2R;

  true_long += (0.00134 * Math.cos(corrA) + 0.00154 * Math.cos(corrB) + 0.002 * Math.cos(corrC) + 0.00179 * Math.sin(corrD) + 0.00178 * Math.sin(corrE)) * D2R;

   // Apparent longitude of the Sun
  var lambda = true_long - (0.00569 + 0.00479 * Math.sin(omega)) * D2R;
  lambda = revrad(lambda);

  var lambda_d = lambda * R2D;
  var z1 = ((lambda_d - Math.floor(lambda_d)) * 60);
  var z2 = ((z1- Math.floor(z1)) * 60);
  document.getElementById("longsun").innerHTML = Math.floor(lambda_d) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';
  if (language == "fr")
    document.getElementById("latsun").innerHTML = "0\xB0&nbsp;00'&nbsp;00,00\"";
  else
    document.getElementById("latsun").innerHTML = "0\xB0&nbsp;00'&nbsp;00.00\"";

   // Sun-Planet Radius vector (AU)
  var R = 1.0000002 * (1 - (e * e)) / (1 + (e * Math.cos(v)));
  R += 0.00000543 * Math.sin(corrA) + 0.00001575 * Math.sin(corrB) + 0.00001627 * Math.sin(corrC) + 0.00003076 * Math.cos(corrD) + 0.00000927 * Math.sin(corrH);

  document.getElementById("rayonsun").innerHTML = fixedn(R, 7) + ((language == "fr") ? "&nbsp;UA" : "&nbsp;AU");

  if ( (lambda_d > 299.55) && (lambda_d <= 327.72) )
  {
    if (language == "fr")
     constell = "Capricorne (Capricornus)";
    else
     constell = "Capricorn (Capricornus)";
  }
  else if ( (lambda_d > 327.72) && (lambda_d <= 351.41) )
  {
    if (language == "fr")
     constell = "Verseau (Aquarius)";
    else
     constell = "Pitcher (Aquarius)";
  }
  else if ( (lambda_d > 351.41) || (lambda_d <= 28.92) )
  {
    if (language == "fr")
     constell = "Poissons (Pisces)";
    else
     constell = "Fish (Pisces)";
  }
  else if ( (lambda_d > 28.92) && (lambda_d <= 53.30) )
  {
    if (language == "fr")
     constell = "B\xE9lier (Aries)";
    else
     constell = "Ram (Aries)";
  }
  else if ( (lambda_d > 53.30) && (lambda_d <= 90.25) )
  {
    if (language == "fr")
     constell = "Taureau (Taurus)";
    else
     constell = "Bull (Taurus)";
  }
  else if ( (lambda_d > 90.25) && (lambda_d <= 118.08) )
  {
    if (language == "fr")
     constell = "G\xE9meaux (Gemini)";
    else
     constell = "Twins (Gemini)";
  }
  else if ( (lambda_d > 118.08) && (lambda_d <= 138.00) )
  {
    if (language == "fr")
     constell = "Cancer (Cancer)";
    else
     constell = "Crab (Cancer)";
  }
  else if ( (lambda_d > 138.00) && (lambda_d <= 173.98) )
  {
    if (language == "fr")
     constell = "Lion (Leo)";
    else
     constell = "Lion (Leo)";
  }
  else if ( (lambda_d > 173.98) && (lambda_d <= 217.62) )
  {
    if (language == "fr")
     constell = "Vierge (Virgo)";
    else
     constell = "Virgin (Virgo)";
  }
  else if ( (lambda_d > 217.62) && (lambda_d <= 240.96) )
  {
    if (language == "fr")
     constell = "Balance (Libra)";
    else
     constell = "Scale (Libra)";
  }
  else if ( (lambda_d > 240.96) && (lambda_d <= 247.86) )
  {
    if (language == "fr")
     constell = "Scorpion (Scorpius)";
    else
     constell = "Scorpion (Scorpio)";
  }
  else if ( (lambda_d > 247.86) && (lambda_d <= 266.42) )
  {
    if (language == "fr")
     constell = "Ophiuchus (Ophiuchus)";
    else
     constell = "Ophiuchus (Ophiuchus)";
  }
  else if ( (lambda_d > 266.42) && (lambda_d <= 299.55) )
  {
    if (language == "fr")
     constell = "Sagittaire (Sagittarius)";
    else
     constell = "Centaur (Sagittarius)";
  }

  document.getElementById("constell").innerHTML = constell;

   // True and apparent obliquity of the ecliptic
  var true_obliquity = (23.452294 - 0.0130125 * T - 0.00000164 * T2 + 0.000000503 * T3) * D2R;
  var obliquity = true_obliquity + (0.00256 * Math.cos(omega)) * D2R;

   // Right Ascension of the Sun
  RADecSun.geoRA = Math.atan(Math.cos(obliquity) * Math.sin(lambda) / Math.cos(lambda));
  if ( RADecSun.geoRA < 0 )
    RADecSun.geoRA += PIx2;
  if ( Math.cos(lambda) < 0 )
    RADecSun.geoRA += Math.PI;
  if ( RADecSun.geoRA > PIx2 )
    RADecSun.geoRA -= PIx2;

   // Declination of the Sun
  RADecSun.geoDec = Math.asin(Math.sin(obliquity) * Math.sin(lambda));

  geocentric2topocentricEquatorial(RADecSun, latObs, lonObs, elvObs, R, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecSun, "alphasun", "deltasun");

  var angularsizesun = 31.9877 / R;  // 33.497406201826
  document.getElementById("angularsizesun").innerHTML = Math.floor(angularsizesun) + "'&nbsp;" + zeroFixed2((angularsizesun - Math.floor(angularsizesun)) * 60) + '"';

   // Equation of Time
  var eqT = (L * R2D) - 0.0057183 - (RADecSun.geoRA * R2D) + nutationRA_corr;
  eqT *= 4.0;	// Degrees to minutes
   // W.M. SMART formula - "Text-Book on Spherical Astronomy" page 19 - ed. 1956 - less accurate
/*  var yT = Math.pow(Math.tan(obliquity / 2), 2);
  var eqT = (yT * Math.sin(2 * L)) - (2 * e * Math.sin(M)) + (4 * e * yT * Math.sin(M) * Math.cos(2 * L)) - (0.5 * yT * yT * Math.sin(4 * L)) - (5 * e * e * Math.sin(2 * M) / 4);
  eqT *= R2D * 4.0;*/
  if (language == "fr")
    eqT = -eqT;	// French Almanacs and older textbooks
  if ( eqT < -20.0 )
    eqT += 1440.0;
  else if ( eqT > 20.0 )
    eqT -= 1440.0;
  document.getElementById("eqT").innerHTML = fixedn(eqT, 4);  // In minutes of time

   //--------
   // Planets
   //--------

  var longitude_sun = lambda;
  var longitude_earth = longitude_sun + Math.PI;
  var latitude_earth = 0;
  var distEarthSun = R;
  distEarthRadii[0] = R * AU / ER;

   //--------
   // Mercury
   //--------

  var const1 = 178.179078;
  var const2 = 149474.07078;
  var const3 = 0.0003011;
  var const4 = 0.38709830982;
  var const5 = 0.20561421;
  var const6 = 0.00002046;
  var const7 = -0.00000003;
  var const8 = 7.002881;
  var const9 = 0.0018608;
  var const10 = -0.0000183;
  var const11 = 102.27938;
  var const12 = 149472.51529;
  var const13 = 0.000007;
  var const14 = 47.145944;
  var const15 = 1.1852083;
  var const16 = 0.0001739;
  var const17 = 6.728;

   // Mean longitude
  var l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  var node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ ) // Normally check that it converge and stop when it differs by less than 0.000001 radians
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Mercury Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));
  r += 0.000007525 * Math.cos(2 * m5 - m1 + 53.013 * D2R);
  r += 0.000006802 * Math.cos(5 * m2 - 3 * m1 - 259.918 * D2R);
  r += 0.000005457 * Math.cos(2 * m2 - 2 * m1 - 71.188 * D2R);
  r += 0.000003569 * Math.cos(5 * m2 - m1 - 77.75 * D2R);

   // Latitude
  u = l + v - m - node_longitude;
  u = revrad(u);
  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  l += 0.00204 * D2R * Math.cos(5 * m2 - 2 * m1 + 12.22 * D2R);
  l += 0.00103 * D2R * Math.cos(2 * m2 - m1 - 160.692 * D2R);
  l += 0.00091 * D2R * Math.cos(2 * m5 - m1 - 37.003 * D2R);
  l += 0.00078 * D2R * Math.cos(5 * m2 - 3 * m1 + 10.137 * D2R);

  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  var numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  var denominateur = (r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI)) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  var diameter = const17;

   // Rectangular to polar coordinates
/*  var xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  var yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  var zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[2] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametremerc", "distancemerc", "rayonmerc");

   // Elongation
  l -= (0.00479 * D2R * Math.sin(omega));

  var elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if ( ((l < longitude_sun) && (longitude_sun - l < Math.PI)) || ((l > longitude_sun) && (l - longitude_sun > Math.PI)) )
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatmerc").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatmercEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 10, language);
  document.getElementById("visibmerc").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  var beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longmerc", "latmerc");

  geocentricEquatorial(RADecMerc, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecMerc, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecMerc, "alphamerc", "deltamerc");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -0.36 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.027 * phase_angle) + (2.2E-13 * Math.pow(phase_angle, 6));

  document.getElementById("magmerc").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phasemerc").innerHTML = fixedn(phase, 1) + "%";

   //------
   // Venus
   //------

  const1 = 342.767053;
  const2 = 58519.21191;
  const3 = 0.0003097;
  const4 = 0.72332981996;
  const5 = 0.00682069;
  const6 = -0.00004774;
  const7 = 0.000000091;
  const8 = 3.393631;
  const9 = 0.0010058;
  const10 = -0.000001;
  const11 = 212.60322;
  const12 = 58517.80387;
  const13 = 0.001286;
  const14 = 75.779647;
  const15 = 0.89985;
  const16 = 0.00041;
  const17 = 16.688;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

  l += 0.00077 * D2R * Math.sin(237.24 * D2R + 150.27 * D2R * T);
  m += 0.00077 * D2R * Math.sin(237.24 * D2R + 150.27 * D2R * T);

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Venus Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));
  r += 0.000022501 * Math.cos(2 * m - 2 * m2 - 58.208 * D2R);
  r += 0.000019045 * Math.cos(3 * m - 3 * m2 + 92.577 * D2R);
  r += 0.000006887 * Math.cos(m5 - m2 - 118.09 * D2R);
  r += 0.000005172 * Math.cos(m - m2 - 29.11 * D2R);
  r += 0.00000362 * Math.cos(5 * m - 4 * m2 - 104.208 * D2R);
  r += 0.000003283 * Math.cos(4 * m - 4 * m2 + 63.513 * D2R);
  r += 0.000003074 * Math.cos(2 * m5 - 2 * m2 - 55.167 * D2R);

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  l += 0.00313 * D2R * Math.cos(2 * m - 2 * m2 - 148.225 * D2R);
  l += 0.00198 * D2R * Math.cos(3 * m - 3 * m2 + 2.565 * D2R);
  l += 0.00136 * D2R * Math.cos(m - m2 - 119.107 * D2R);
  l += 0.00096 * D2R * Math.cos(3 * m - 2 * m2 - 135.912 * D2R);
  l += 0.00082 * D2R * Math.cos(m5 - m2 - 208.087 * D2R);

  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[3] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametreven", "distanceven", "rayonven");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatven").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatvenEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 10, language);
  document.getElementById("visibven").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longven", "latven");

  geocentricEquatorial(RADecVen, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecVen, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecVen, "alphaven", "deltaven");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -4.34 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.013 * phase_angle) + (4.2E-7 * Math.pow(phase_angle, 3));

  document.getElementById("magven").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phaseven").innerHTML = fixedn(phase, 1) + "%";

   //-----
   // Mars
   //-----

  const1 = 293.737334;
  const2 = 19141.69551;
  const3 = 0.0003107;
  const4 = 1.52367934191;
  const5 = 0.0933129;
  const6 = 0.000092064;
  const7 = -0.000000077;
  const8 = 1.850333;
  const9 = -0.000675;
  const10 = 0.0000126;
  const11 = 319.51913;
  const12 = 19139.85475;
  const13 = 0.000181;
  const14 = 48.786442;
  const15 = 0.7709917;
  const16 = -0.0000014;
  const17 = 9.368;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

   // Corrections
  corr = -0.01133 * D2R * Math.sin(3 * m5 - 8 * m4 + 4 * m) - 0.00933 * D2R * Math.cos(3 * m5 - 8 * m4 + 4 * m);
  l += corr;
  m += corr;

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Mars Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));
  r += 0.000053227 * Math.cos(m5 - m4 + 41.1306 * D2R);
  r += 0.000050989 * Math.cos(2 * m5 - 2 * m4 - 101.9847 * D2R);
  r += 0.000038278 * Math.cos(2 * m5 - m4 - 98.3292 * D2R);
  r += 0.000015996 * Math.cos(m - m4 - 55.555 * D2R);
  r += 0.000014764 * Math.cos(2 * m - 3 * m4 + 68.622 * D2R);
  r += 0.000008966 * Math.cos(m5 - 2 * m4 + 43.615 * D2R);
  r += 0.000007914 * Math.cos(3 * m5 - 2 * m4 - 139.737 * D2R);
  r += 0.000007004 * Math.cos(2 * m5 - 3 * m4 - 102.888 * D2R);
  r += 0.00000662 * Math.cos(m - 2 * m4 + 113.202 * D2R);
  r += 0.00000493 * Math.cos(3 * m5 - 3 * m4 - 76.243 * D2R);
  r += 0.000004693 * Math.cos(3 * m - 5 * m4 + 190.603 * D2R);
  r += 0.000004571 * Math.cos(2 * m - 4 * m4 + 244.702 * D2R);
  r += 0.000004409 * Math.cos(3 * m5 - m4 - 115.828 * D2R);

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  l += 0.00705 * D2R * Math.cos(m5 - m4 - 48.958 * D2R);
  l += 0.00607 * D2R * Math.cos(2 * m5 - m4 - 188.35 * D2R);
  l += 0.00445 * D2R * Math.cos(2 * m5 - 2 * m4 - 191.897 * D2R);
  l += 0.00388 * D2R * Math.cos(m - 2 * m4 + 20.495 * D2R);
  l += 0.00238 * D2R * Math.cos(m - m4 + 35.097 * D2R);
  l += 0.00204 * D2R * Math.cos(2 * m - 3 * m4 + 158.638 * D2R);
  l += 0.00177 * D2R * Math.cos(3 * m4 - m2 - 57.602 * D2R);
  l += 0.00136 * D2R * Math.cos(2 * m - 4 * m4 + 154.093 * D2R);
  l += 0.00104 * D2R * Math.cos(m5 + 17.618 * D2R);

  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[4] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametremars", "distancemars", "rayonmars");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatmars").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatmarsEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 10, language);
  document.getElementById("visibmars").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longmars", "latmars");

  geocentricEquatorial(RADecMars, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecMars, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecMars, "alphamars", "deltamars");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -1.51 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.016 * phase_angle);

  document.getElementById("magmars").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phasemars").innerHTML = fixedn(phase, 1) + "%";

   //--------
   // Jupiter
   //--------

  const1 = 238.049257;
  const2 = 3036.301986;
  const3 = 0.0003347;
  const4 = 5.20260300002;
  const5 = 0.04833475;
  const6 = 0.00016418;
  const7 = -0.0000004676;
  const8 = 1.308736;
  const9 = -0.0056961;
  const10 = 0.0000039;
  const11 = 225.32833;
  const12 = 3034.69202;
  const13 = -0.000722;
  const14 = 99.443414;
  const15 = 1.01053;
  const16 = 0.00035222;
  const17 = 197.146;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

   // Periodical terms
  u = T / 5 + 0.1;
  p = (237.475 + 3034.9061 * T) * D2R;
  q = (265.916 + 1222.1139 * T) * D2R;
  v = 5 * q - 2 * p;
  dzeta = q - p;

   // Mean longitude perturbations
  granda = (0.3314 - 0.0103 * u - 0.0047 * u * u) * Math.sin(v);
  granda += (0.0032 - 0.0644 * u + 0.0021 * u * u) * Math.cos(v);
  granda += 0.0136 * Math.sin(dzeta) + 0.0185 * Math.sin(2 * dzeta) + 0.0067 * Math.sin(3 * dzeta);
  granda += (0.0073 * Math.sin(dzeta) + 0.0064 * Math.sin(2 * dzeta) - 0.0338 * Math.cos(dzeta)) * Math.sin(q);
  granda -= (0.0357 * Math.sin(dzeta) + 0.0063 * Math.cos(dzeta) + 0.0067 * Math.cos(2 * dzeta)) * Math.cos(q);

   // Ecccentricity perturbations
  grandb = (361 + 13 * u) * Math.sin(v) + (129 - 58 * u) * Math.cos(v);
  grandb += (128 * Math.cos(dzeta) - 676 * Math.sin(dzeta) - 111 * Math.sin(2 * dzeta)) * Math.sin(q);
  grandb += (146 * Math.sin(dzeta) - 82 + 607 * Math.cos(dzeta) + 99 * Math.cos(2 * dzeta) + 51 * Math.cos(3 * dzeta)) * Math.cos(q);
  grandb -= (96 * Math.sin(dzeta) + 100 * Math.cos(dzeta)) * Math.sin(2 * q) - (96 * Math.sin(dzeta) - 102 * Math.cos(dzeta)) * Math.cos(2 * q);

   // Perihelion longitude perturbations
  grandc = (0.0072 - 0.0031 * u) * Math.sin(v) - 0.0204 * Math.cos(v);
  grandc += (0.0073 * Math.sin(dzeta) + 0.034 * Math.cos(dzeta) + 0.0056 * Math.cos(2 * dzeta)) * Math.sin(q);
  grandc += (0.0378 * Math.sin(dzeta) + 0.0062 * Math.sin(2 * dzeta) - 0.0066 * Math.cos(dzeta)) * Math.cos(q);
  grandc -= 0.0054 * Math.sin(dzeta) * Math.sin(2 * q) + 0.0055 * Math.cos(dzeta) * Math.cos(2 * q);

   // Semimajor axis perturbations
  grandd = -263 * Math.cos(v) + 205 * Math.cos(dzeta) + 693 * Math.cos(2 * dzeta) + 312 * Math.cos(3 * dzeta) + 299 * Math.sin(dzeta) * Math.sin(q) + (204 * Math.sin(2 * dzeta) - 337 * Math.cos(dzeta)) * Math.cos(q);

   // Corrections
  l += granda * D2R;
  m += granda * D2R - grandc * D2R / e;
  e += grandb / 1000000;
  a += grandd / 1000000;

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Jupiter Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[5] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametrejup", "distancejup", "rayonjup");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatjup").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatjupEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 20, language);
  document.getElementById("visibjup").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longjup", "latjup");

  geocentricEquatorial(RADecJup, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecJup, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecJup, "alphajup", "deltajup");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -9.25 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.014 * phase_angle);

  document.getElementById("magjup").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phasejup").innerHTML = fixedn(phase, 1) + "%";

   //-------
   // Saturn
   //-------

  const1 = 266.564377;
  const2 = 1223.509884;
  const3 = 0.0003245;
  const4 = 9.55491173474;
  const5 = 0.05589232;
  const6 = -0.0003455;
  const7 = -0.000000728;
  const8 = 2.492519;
  const9 = -0.0039189;
  const10 = -0.00001549;
  const11 = 175.46622;
  const12 = 1221.55147;
  const13 = -0.000502;
  const14 = 112.790414;
  const15 = 0.8731951;
  const16 = -0.00015218;
  const17 = 166.194;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

   // Periodical terms
  u = T / 5 + 0.1;
  p = (237.475 + 3034.9061 * T) * D2R;
  q = (265.916 + 1222.1139 * T) * D2R;
  v = 5 * q - 2 * p;
  dzeta = q - p;

   // Mean longitude perturbations
  granda = (-0.8142 + 0.0181 * u + 0.0167 * u * u) * Math.sin(v);
  granda += (-0.0105 + 0.1609 * u - 0.0041 * u * u) * Math.cos(v);
  granda -= 0.1488 * Math.sin(dzeta) - 0.0408 * Math.sin(2 * dzeta) - 0.0152 * Math.sin(3 * dzeta);
  granda += (0.0089 * Math.sin(dzeta) - 0.0165 * Math.sin(2 * dzeta)) * Math.sin(q);
  granda += (0.0813 * Math.cos(dzeta) + 0.015 * Math.cos(2 * dzeta)) * Math.sin(q);
  granda += (0.0856 * Math.sin(dzeta) + 0.0253 * Math.cos(dzeta) + 0.0144 * Math.cos(2 * dzeta)) * Math.cos(q);
  granda += 0.0092 * Math.sin(2 * dzeta) * Math.sin(2 * q);

   // Ecccentricity perturbations
  grandb = (-793 + 255 * u) * Math.sin(v) + (1338 + 123 * u) * Math.cos(v);
  grandb += 1241 * Math.sin(q) + (39 - 62 * u) * Math.sin(dzeta) * Math.sin(q);
  grandb += (2660 * Math.cos(dzeta) - 469 * Math.cos(2 * dzeta) - 187 * Math.cos(3 * dzeta) - 82 * Math.cos(4 * dzeta)) * Math.sin(q);
  grandb -= (1270 * Math.sin(dzeta) + 420 * Math.sin(2 * dzeta) + 150 * Math.sin(3 * dzeta)) * Math.cos(q);
  grandb -= 62 * Math.sin(4 * dzeta) * Math.cos(q);
  grandb += (221 * Math.sin(dzeta) - 221 * Math.sin(2 * dzeta) - 57 * Math.sin(3 * dzeta)) * Math.sin(2 * q);
  grandb -= (278 * Math.cos(dzeta) - 202 * Math.cos(2 * dzeta)) * Math.sin(2 * q);
  grandb -= (284 * Math.sin(dzeta) + 159 * Math.cos(dzeta)) * Math.cos(2 * q);
  grandb += (216 * Math.cos(2 * dzeta) + 56 * Math.cos(3 * dzeta)) * Math.cos(2 * q);

   // Perihelion longitude perturbations
  grandc = (0.0771 + 0.0072 * u) * Math.sin(v);
  grandc += (0.0458 - 0.0148 * u) * Math.cos(v);
  grandc = grandc - (0.0758 * Math.sin(dzeta) + 0.0248 * Math.sin(2 * dzeta) + 0.0086 * Math.sin(3 * dzeta)) * Math.sin(q);
  grandc = grandc - (0.0726 + 0.1504 * Math.cos(dzeta) - 0.0269 * Math.cos(2 * dzeta) - 0.0101 * Math.cos(3 * dzeta)) * Math.cos(q);
  grandc = grandc - (0.0136 * Math.sin(dzeta) - 0.0136 * Math.cos(2 * dzeta)) * Math.sin(2 * q);
  grandc = grandc - (0.0137 * Math.sin(dzeta) - 0.012 * Math.sin(2 * dzeta)) * Math.cos(2 * q);
  grandc += (0.0149 * Math.cos(dzeta) - 0.0131 * Math.cos(2 * dzeta)) * Math.cos(2 * q);

   // Semimajor axis perturbations
  grandd = 2933 * Math.cos(v) + 33629 * Math.cos(dzeta) - 3081 * Math.cos(2 * dzeta) - 1423 * Math.cos(3 * dzeta) - 671 * Math.cos(4 * dzeta) + (1098 - 2812 * Math.sin(dzeta) + 688 * Math.sin(2 * dzeta)) * Math.sin(q);
  grandd = grandd + (2138 * Math.cos(dzeta) - 999 * Math.cos(2 * dzeta) - 642 * Math.cos(3 * dzeta)) * Math.sin(q) - 890 * Math.cos(q) + (2206 * Math.sin(dzeta) - 1590 * Math.sin(2 * dzeta) - 647 * Math.sin(3 * dzeta)) * Math.cos(q) + (2885 * Math.cos(dzeta) + 2172 * Math.cos(2 * dzeta)) * Math.cos(q) - 778 * Math.cos(dzeta) * Math.sin(2 * q) - 856 * Math.sin(dzeta) * Math.cos(2 * q);

   // Corrections
  l += granda * D2R;
  m += granda * D2R - grandc * D2R / e;
  e += grandb / 1000000;
  a += grandd / 1000000;

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Saturn Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  corr = 0.000747 * Math.cos(dzeta) * Math.sin(q) + 0.001069 * Math.cos(dzeta) * Math.cos(q);
  corr += 0.002108 * Math.sin(2 * dzeta) * Math.sin(2 * q) + 0.001261 * Math.cos(2 * dzeta) * Math.sin(2 * q);
  corr += 0.001236 * Math.sin(2 * dzeta) * Math.cos(2 * q) - 0.002075 * Math.cos(2 * dzeta) * Math.cos(2 * q);
  b += corr * D2R;

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[6] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametresat", "distancesat", "rayonsat");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatsat").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatsatEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 20, language);
  document.getElementById("visibsat").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longsat", "latsat");

  geocentricEquatorial(RADecSat, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecSat, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecSat, "alphasat", "deltasat");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -9.0 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.044 * phase_angle);
  var ir = 28.06 * D2R;
  var Nr = (169.51 * D2R) + ((3.82E-5 * D2R) * T);
  var B = Math.asin(Math.sin(beta) * Math.cos(ir) - Math.cos(beta) * Math.sin(ir) * Math.sin(l - Nr));
  ring_magnitude = -(2.6 * Math.sin(Math.abs(B))) + (1.2 * Math.pow(Math.sin(B), 2));

  document.getElementById("magsat").innerHTML = fixedn(magnitude + ring_magnitude, 2);
  document.getElementById("phasesat").innerHTML = fixedn(phase, 1) + "%";

   //-------
   // Uranus
   //-------

  const1 = 244.19747;
  const2 = 429.863546;
  const3 = 0.000316;
  const4 = 19.21844609894;
  const5 = 0.0463444;
  const6 = -0.00002658;
  const7 = 0.000000077;
  const8 = 0.772464;
  const9 = 0.0006253;
  const10 = 0.0000395;
  const11 = 72.648778;
  const12 = 428.3791132;
  const13 = 0.0000788;
  const14 = 73.477111;
  const15 = 0.4986678;
  const16 = 0.0013117;
  const17 = 70.481;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Mean anomaly
  m1 = (102.27938 + (149472.51529 * T) + (0.000007 * T2)) * D2R;
  m1 = revrad(m1);
  m2 = (212.60322 + (58517.80387 * T) + (0.001286 * T2)) * D2R;
  m2 = revrad(m2);
  m4 = (319.51913 + (19139.85475 * T) + (0.000181 * T2)) * D2R;
  m4 = revrad(m4);
  m5 = (225.32833 + (3034.69202 * T) - (0.000722 * T2)) * D2R;
  m5 = revrad(m5);
  m6 = (175.46622 + (1221.55147 * T) - (0.000502 * T2)) * D2R;
  m6 = revrad(m6);

   // Periodical terms
  u = T / 5 + 0.1;
  p = (237.475 + (3034.9061 * T)) * D2R;
  q = (265.916 + (1222.1139 * T)) * D2R;
  g = (83.76922 + (218.4901 * T)) * D2R;
  h = (284.02 + (8.51 * T)) * D2R;
  s = (243.52 + 428.47 * T) * D2R;
  w = 2 * p - 6 * q + 3 * s;
  dzeta = s - p;
  eta = s - q;
  theta = (200.25 - (209.98 * T)) * D2R;

   // Perturbations
  granda = (0.864319 - 0.001583 * u) * Math.sin(h) + 0.036017 * Math.sin(2 * h) + (0.082222 - 0.006833 * u) * Math.cos(h) - 0.003019 * Math.cos(2 * h) + 0.008122 * Math.sin(w);
  grandb = 2098 * Math.cos(h) - 335 * Math.sin(h) + 131 * Math.cos(2 * h);
  grandc = 0.120303 * Math.sin(h) + (0.019472 - 0.000947 * u) * Math.cos(h) + 0.006197 * Math.sin(2 * h);
  grandd = -3825 * Math.cos(h);

   // Corrections
  l += granda * D2R;
  m += granda * D2R - grandc * D2R / e;
  e += grandb / 1000000;
  a += grandd / 1000000;

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Uranus Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));
  r -= 0.025948 + 0.004985 * Math.cos(dzeta) - 0.00123 * Math.cos(s) + 0.003354 * Math.cos(eta);
  r += (0.005795 * Math.cos(s) - 0.001165 * Math.sin(s) + 0.001388 * Math.cos(2 * s)) * Math.sin(eta);
  r += (0.001351 * Math.cos(s) + 0.005702 * Math.sin(s) + 0.00138 * Math.sin(2 * s)) * Math.cos(eta);
  r += 0.000904 * Math.cos(2 * theta) + 0.000894 * (Math.cos(theta) - Math.cos(3 * theta));

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  l += ((0.010122 - 0.000988 * u) * Math.sin(s + eta) + (-0.038581 + 0.002031 * u - 0.00191 * u * u) * Math.cos(s + eta) + (0.034964 - 0.001038 * u + 0.000868 * u * u) * Math.cos(2 * s + eta) - 0.014808 * Math.sin(dzeta) - 0.005794 * Math.sin(eta) + 0.002347 * Math.cos(eta) + 0.009872 * Math.sin(theta) + 0.008803 * Math.sin(2 * theta) - 0.004308 * Math.sin(3 * theta)) * D2R;
  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));

  corr = (0.000458 * Math.sin(eta) - 0.000642 * Math.cos(eta) - 0.000517 * Math.cos(4 * theta)) * Math.sin(s);
  corr -= (0.000347 * Math.sin(eta) + 0.000853 * Math.cos(eta) + 0.000517 * Math.sin(4 * eta)) * Math.cos(s);
  corr += 0.000403 * (Math.cos(2 * theta) * Math.sin(2 * s) + Math.sin(2 * theta) * Math.cos(2 * s));
  b += corr * D2R;

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[7] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametreuran", "distanceuran", "rayonuran");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongaturan").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongaturanEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 20, language);
  document.getElementById("visiburan").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longuran", "laturan");

  geocentricEquatorial(RADecUran, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecUran, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecUran, "alphauran", "deltauran");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -7.15 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.001 * phase_angle);

  document.getElementById("maguran").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phaseuran").innerHTML = fixedn(phase, 1) + "%";

   //--------
   // Neptune
   //--------

  const1 = 84.457994;
  const2 = 219.885914;
  const3 = 0.0003205;
  const4 = 30.11038703542;
  const5 = 0.00899704;
  const6 = 0.00000633;
  const7 = -0.000000002;
  const8 = 1.779242;
  const9 = -0.0095436;
  const10 = -0.0000091;
  const11 = 37.73063;
  const12 = 218.4613396;
  const13 = -0.00007032;
  const14 = 130.681389;
  const15 = 1.098935;
  const16 = 0.00024987;
  const17 = 68.289;

   // Mean longitude
  l = (const1 + const2 * T + const3 * T2) * D2R;
  l = revrad(l);

   // Semimajor axis
  a = const4;

   // Eccentricity (e), inclinaison (i), ascending node longitude (m)
  e = const5 + (const6 * T) + (const7 * T2);
  i = (const8 + (const9 * T) + (const10 * T2)) * D2R;
  m = (const11 + (const12 * T) + (const13 * T2)) * D2R;
  m = revrad(m);
  node_longitude = (const14 + (const15 * T) + (const16 * T2)) * D2R;

   // Periodical terms
  u = T / 5 + 0.1;
  p = (237.475 + (3034.9061 * T)) * D2R;
  q = (265.916 + (1222.1139 * T)) * D2R;
  g = (83.76922 + (218.4901 * T)) * D2R;
  h = (284.02 + (8.51 * T)) * D2R;
  theta = (200.25 - (209.98 * T)) * D2R;
  dzeta = (153.71 + 2816.42 * T) * D2R;
  eta = (182.15 + 1003.62 * T) * D2R;

   // Perturbations
  granda = (-0.589833 + 0.001089 * u) * Math.sin(h) + (-0.056094 + 0.004658 * u) * Math.cos(h) - 0.024286 * Math.sin(2 * h);
  grandb = 438.9 * Math.sin(h) + 426.2 * Math.cos(h) + 112.9 * Math.sin(2 * h) + 108.9 * Math.cos(2 * h);
  grandc = 0.024039 * Math.sin(h) - 0.025303 * Math.cos(h) + 0.006206 * Math.sin(2 * h) - 0.005992 * Math.cos(2 * h);
  grandd = -817 * Math.sin(h) + 8189 * Math.cos(h) + 781 * Math.cos(2 * h);

   // Corrections
  l += granda * D2R;
  m += granda * D2R - grandc * D2R / e;
  e += grandb / 1000000;
  a += grandd / 1000000;

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Neptune Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));
  r -= 0.040596 + 0.004992 * Math.cos(dzeta) + 0.002744 * Math.cos(eta) + 0.002044 * Math.cos(theta) + 0.001051 * Math.cos(2 * theta);

   // Latitude argument
  u = l + v - m - node_longitude;
  u = revrad(u);

  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + node_longitude;
  l += (0.009556 * Math.sin(dzeta) + 0.005178 * Math.sin(eta)) * D2R;
  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));
  b += (0.000336 * Math.cos(2 * theta) * Math.sin(g) + 0.000364 * Math.sin(2 * theta) * Math.cos(g)) * D2R;

  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[8] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diametrenept", "distancenept", "rayonnept");

   // Elongation
  l -= 0.00479 * D2R * Math.sin(omega);

  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatnept").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatneptEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 20, language);
  document.getElementById("visibnept").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longnept", "latnept");

  geocentricEquatorial(RADecNept, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecNept, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecNept, "alphanept", "deltanept");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -6.90 + (5.0 * (Math.log(r * distFromEarth)) / Math.log(10.0)) + (0.001 * phase_angle);

  document.getElementById("magnept").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phasenept").innerHTML = fixedn(phase, 1) + "%";

   //------
   // Pluto (http://ssd.jpl.nasa.gov/?planets#elem)
   //------

  const17 = 3.200;

   // Mean longitude
  l = mod2pi((238.92881 + (522747.90 * T_J2000 / 3600)) * D2R);
  l = revrad(l);

   // Semimajor axis
  a = 39.48168677 - (0.00076912 * T_J2000);

   // Eccentricity (e), inclinaison (i), perihelion argument (ap), ascending node longitude (Om)
  e = 0.24880766 + (0.00006465 * T_J2000);
  i = (17.14175 + (11.07 * T_J2000 / 3600)) * D2R;
  ap = (224.06676 - (132.25 * T_J2000 / 3600)) * D2R;
  Om = (110.30347 - (37.33 * T_J2000 / 3600)) * D2R;

   // Perihelion longitude
  m = l - ap;
  m = revrad(m);

   // Kepler equation
  eccentric_anomaly = m;
  for ( tmp = 1; tmp < 11; tmp++ )
    eccentric_anomaly = m + (e * Math.sin(eccentric_anomaly));

   // True anomaly
  v = 2 * Math.atan(Math.sqrt((1 + e) / (1 - e)) * Math.tan(eccentric_anomaly / 2));
  if ( v < 0 )
    v += PIx2;

   // Sun-Pluto Radius vector (AU)
  r = a * (1 - (e * Math.cos(eccentric_anomaly)));

   // Latitude argument
  u = l + v - m - Om;
  u = revrad(u);
  if ( Math.cos(u) != 0 )
  {
    d = Math.atan(Math.cos(i) * Math.tan(u));
    if ( Math.cos(u) < 0 )
      d += Math.PI;
  }
  else
    d = u;

   // Ecliptic longitude
  l = d + Om;
  if ( l > PIx2 )
    l -= PIx2;

  b = Math.asin(Math.sin(u) * Math.sin(i));
  numerateur = r * Math.cos(b) * Math.sin(l - longitude_earth + Math.PI);
  denominateur = r * Math.cos(b) * Math.cos(l - longitude_earth + Math.PI) + distEarthSun;

  l = Math.atan(numerateur / denominateur) + longitude_earth + Math.PI;
  if ( l > PIx2 )
    l -= PIx2;
  if ( denominateur < 0 )
    l += Math.PI;

  diameter = const17;

   // Rectangular to polar coordinates
/*  xp = r * Math.cos(b) * Math.cos(l) - distEarthSun * Math.cos(latitude_earth) * Math.cos(longitude_earth);
  yp = r * Math.cos(b) * Math.sin(l) - distEarthSun * Math.cos(latitude_earth) * Math.sin(longitude_earth);
  zp = r * Math.sin(b) - distEarthSun * Math.sin(latitude_earth);*/

  distFromEarth = Math.sqrt((numerateur * numerateur) + (denominateur * denominateur) + (r * r * Math.sin(b) * Math.sin(b)));
  distEarthRadii[9] = distFromEarth * AU / ER;

  showApparentDiameterDistance(language, diameter, distFromEarth, r, "diameterpluto", "distancepluto", "radiuspluto");

   // Elongation
  var omega2 = (259.18 - (1934.142 * T_J2000)) * D2R;
  l -= 0.00479 * Math.sin(omega2) * D2R;
  elongation = R2D * Math.acos(Math.cos(b) * Math.cos(l - longitude_sun));
  if (((l < longitude_sun) && ((longitude_sun - l) < Math.PI)) || ((l > longitude_sun) && ((l - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      planet_elongation = "Ouest";
    else
      planet_elongation = "West";
  }
  else
  {
    if (language == "fr")
      planet_elongation = "Est";
    else
      planet_elongation = "East";
  }

  document.getElementById("elongatpluto").innerHTML = fixedn(elongation, 1) + "\xB0 ";
  document.getElementById("elongatplutoEW").innerHTML = planet_elongation;

  visible_when_where(planet_elongation, elongation, 20, language);
  document.getElementById("visibpluton").innerHTML = gVisibility + " " + gWhen;

   // Convert longitude and latitude to geocentric right ascension and declination
  l = revrad(l);
  beta = Math.asin(r * Math.sin(b) / distFromEarth);

  showEclipticCoordinates(l, beta, "longpluto", "latpluto");

  geocentricEquatorial(RADecPluto, obliquity, l, beta);
  geocentric2topocentricEquatorial(RADecPluto, latObs, lonObs, elvObs, distFromEarth, jd, jde, false);
  showGeocentricEquatorialCoordinates(RADecPluto, "alphapluto", "deltapluto");

   // Apparent magnitude
  phase_angle = Math.acos(((r * r) + (distFromEarth * distFromEarth) - (R * R)) / (2.0 * r * distFromEarth)) * R2D;
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  magnitude = -0.14 + (5.0 * Math.log(r * distFromEarth) / Math.log(10.0));

  document.getElementById("magpluto").innerHTML = fixedn(magnitude, 2);
  document.getElementById("phasepluto").innerHTML = fixedn(phase, 1) + "%";

   //-----
   // Moon
   //-----

  e = 1.0 - (0.002516 * T_J2000) - (0.0000074 * T2_J2000);
//  eccentricity = e * 0.01675104;

/*  mercury = (252.250906 + 149472.674636 * T_J2000) * D2R;
  mercury = revrad(mercury);*/
  venus = (181.979801 + 58517.815676 * T_J2000) * D2R;
  venus = revrad(venus);
  mean_longitude_earth = (100.46645 + 35999.372854 * T_J2000) * D2R;
  mean_longitude_earth = revrad(mean_longitude_earth);
  mars = (355.433275 + 19140.299331 * T_J2000) * D2R;
  mars = revrad(mars);
  jupiter = (34.351484 + 3034.905675 * T_J2000) * D2R;
  jupiter = revrad(jupiter);
/*  saturn = (50.077471 + 1222.113794 * T_J2000) * D2R;
  saturn = revrad(saturn);
  uranus = ((314 + 3 / 60 + 18.01841 / 3600) + 1542481.19393 / 3600 * T_J2000) * D2R;
  uranus = revrad(uranus);
  neptune = ((304 + 20 / 60 + 55.19575 / 3600) + 786550.32074 / 3600 * T_J2000) * D2R;
  neptune = revrad(neptune);*/

  a1 = (119.75 + 131.849 * T_J2000) * D2R;
  a2 = (53.09 + 479264.29 * T_J2000) * D2R;
  a3 = (313.45 + 481266.484 * T_J2000) * D2R;

   // Longitude of ascending node
  om = (125.044555 - 1934.1361849 * T_J2000 + 0.0020762 * T2_J2000 + T3_J2000 / 467410 - T4_J2000 / 60616000) * D2R;
  node_longitude = om;

   // Moon's mean longitude
  l = (218.3164591 + 481267.88134236 * T_J2000 - 0.0013268 * T2_J2000 + T3_J2000 / 538841 - T4_J2000 / 65194000) * D2R;
  l = revrad(l);

   // Moon's mean anomaly
  m = (357.5291092 + 35999.0502909 * T_J2000 - 0.0001536 * T2_J2000 + T3_J2000 / 24490000) * D2R;
  anomaly_moon = m;

   // Argument of latitude (mean distance from ascending node)
  f = (93.2720993 + 483202.0175273 * T_J2000 - 0.0034029 * T2_J2000 - T3_J2000 / 3526000 + T4_J2000 / 863310000) * D2R;
  f = revrad(f);

   // Mean anomaly of the Moon
  n = (134.9634114 + 477198.8676313 * T_J2000 + 0.008997 * T2_J2000 + T3_J2000 / 69699 - T4_J2000 / 14712000) * D2R;
  n = revrad(n);
  small_l = n;

   // Mean elongation of the Moon from the Sun
  d = (297.8502042 + 445267.1115168 * T_J2000 - 0.00163 * T2_J2000 + T3_J2000 / 545868 - T4_J2000 / 113065000) * D2R;
  d = revrad(d);

  dzeta_moon = l + 5029.0966 / 3600 * D2R * T_J2000;

  nu = (-171996 - 174.2 * T_J2000) * Math.sin(om);
  nu += (-13187 - 1.6 * T_J2000) * Math.sin(-2 * d + 2 * f + 2 * om);
  nu += (-2274 - 0.2 * T_J2000) * Math.sin(2 * f + 2 * om);
  nu += (2062 + 0.2 * T_J2000) * Math.sin(2 * om);
  nu += (1426 - 3.4 * T_J2000) * Math.sin(m);
  nu += (712 + 0.1 * T_J2000) * Math.sin(n);
  nu += (-517 + 1.2 * T_J2000) * Math.sin(-2 * d + m + 2 * f + 2 * om);
  nu += (-386 - 0.4 * T_J2000) * Math.sin(2 * f + om);
  nu -= 301 * Math.sin(n + 2 * f + 2 * om);
  nu += (217 - 0.5 * T_J2000) * Math.sin(-2 * d - m + 2 * f + 2 * om);
  nu -= 158 * Math.sin(-2 * d + n);
  nu += (129 + 0.1 * T_J2000) * Math.sin(-2 * d + 2 * f + om);
  nu += 123  * Math.sin(-n + 2 * f + 2 * om);
  nu += 63 * Math.sin(2 * d);
  nu += (63 + 0.1 * T_J2000) * Math.sin(n + om);
  nu -= 59 * Math.sin(2 * d - n + 2 * f + 2 * om);
  nu -= (58 + 0.1 * T_J2000) * Math.sin(-n + om);
  nu -= 51 * Math.sin(n + 2 * f + om);
  nu += 48 * Math.sin(-2 * d + 2 * n);
  nu += 46 * Math.sin(-2 * n + 2 * f + om);
  nu -= 38 * Math.sin(2 * (d + f + om));
  nu -= 31 * Math.sin(2 * (n + f + om));
  nu += 29 * Math.sin(2 * n);
  nu += 29 * Math.sin(-2 * d + n + 2 * f + 2 * om);
  nu += 26 * Math.sin(2 * f);
  nu -= 22 * Math.sin(-2 * d + 2 * f);
  nu += 21 * Math.sin(-n + 2 * f + om);
  nu += (17 - 0.1 * T_J2000) * Math.sin(2 * m);
  nu += 16 * Math.sin(2 * d - n + om);
  nu += (-16 + 0.1 * T_J2000) * Math.sin(-2 * d + 2 * m + 2 * f + 2 * om);
  nu -= 15 * Math.sin(m + om);
  nu -= 13 * Math.sin(-2 * d + n + om);
  nu -= 12 * Math.sin(-m + om);
  nu += 11 * Math.sin(2 * n - 2 * f);
  nu -= 10 * Math.sin(2 * d - n + 2 * f + om);
  nu -= 8 * Math.sin(2 * d + n + 2 * f + 2 * om);
  nu += 7 * Math.sin(m + 2 * f + 2 * om);
  nu -= 7 * Math.sin(-2 * d + m + n);
  nu -= 7 * Math.sin(-m + 2 * f + 2 * om);
  nu -= 7 * Math.sin(2 * d + 2 * f + om);
  nu += 6 * Math.sin(2 * d + n);
  nu += 6 * Math.sin(-2 * d + 2 * n + 2 * f + 2 * om);
  nu += 6 * Math.sin(-2 * d + n + 2 * f + om);
  nu -= 6 * Math.sin(2 * d - 2 * n + om);
  nu -= 6 * Math.sin(2 * d + om);
  nu += 5 * Math.sin(-m + n);
  nu -= 5 * Math.sin(-2 * d - m + 2 * f + om);
  nu -= 5 * Math.sin(-2 * d + om);
  nu -= 5 * Math.sin(2 * n + 2 * f + om);
  nu += 4 * Math.sin(-2 * d + 2 * n + om);
  nu += 4 * Math.sin(-2 * d + m + 2 * f + om);
  nu += 4 * Math.sin(n - 2 * f);
  nu -= 4 * Math.sin(-d + n);
  nu -= 4 * Math.sin(-2 * d + m);
  nu -= 4 * Math.sin(d);
  nu += 3 * Math.sin(n + 2 * f);
  nu -= 3 * Math.sin(-2 * n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(-d - m + n);
  nu -= 3 * Math.sin(m + n);
  nu -= 3 * Math.sin(-m + n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(2 * d - m - n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(3 * n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(2 * d - m + 2 * f + 2 * om);
  nutation_longitude = nu / 10000;

  nu = (92025 + 8.9 * T_J2000) * Math.cos(om);
  nu += (5736 - 3.1 * T_J2000) * Math.cos(-2 * d + 2 * f + 2 * om);
  nu += (977 - 0.5 * T_J2000) * Math.cos(2 * f + 2 * om);
  nu += (-895 + 0.5 * T_J2000) * Math.cos(2 * om);
  nu += (54 - 0.1 * T_J2000) * Math.cos(m);
  nu -= 7 * Math.cos(n);
  nu += (224 - 0.6 * T_J2000) * Math.cos(-2 * d + m + 2 * f + 2 * om);
  nu += 200 * Math.cos(2 * f + om);
  nu += (129 - 0.1 * T_J2000) * Math.cos(n + 2 * f + 2 * om);
  nu += (-95 + 0.3 * T_J2000) * Math.cos(-2 * d - m + 2 * f + 2 * om);
  nu -= 70 * Math.cos(-2 * d + 2 * f + om);
  nu -= 53 * Math.cos(-n + 2 * f + 2 * om);
  nu -= 33 * Math.cos(n + om);
  nu += 26 * Math.cos(2 * d - n + 2 * f + 2 * om);
  nu += 32 * Math.cos(-n + om);
  nu += 27 * Math.cos(n + 2 * f + om);
  nu -= 24 * Math.cos(-2 * n + 2 * f + om);
  nu += 16 * Math.cos(2 * (d + f + om));
  nu += 13 * Math.cos(2 * (n + f + om));
  nu -= 12 * Math.cos(-2 * d + n + 2 * f + 2 * om);
  nu -= 10 * Math.cos(-n + 2 * f + om);
  nu -= 8 * Math.cos(2 * d - n + om);
  nu += 7 * Math.cos(-2 * d + 2 * m + 2 * f + 2 * om);
  nu += 9 * Math.cos(m + om);
  nu += 7 * Math.cos(-2 * d + n + om);
  nu += 6 * Math.cos(-m + om);
  nu += 5 * Math.cos(2 * d - n + 2 * f + om);
  nu += 3 * Math.cos(2 * d + n + 2 * f + 2 * om);
  nu -= 3 * Math.cos(m + 2 * f + 2 * om);
  nu += 3 * Math.cos(-m + 2 * f + 2 * om);
  nu += 3 * Math.cos(2 * d + 2 * f + om);
  nu -= 3 * Math.cos(-2 * d + 2 * n + 2 * f + 2 * om);
  nu -= 3 * Math.cos(-2 * d + n + 2 * f + om);
  nu += 3 * Math.cos(2 * d - 2 * n + om);
  nu += 3 * Math.cos(2 * d + om);
  nu += 3 * Math.cos(-2 * d - m + 2 * f + om);
  nu += 3 * Math.cos(-2 * d + om);
  nu += 3 * Math.cos(2 * n + 2 * f + om);
  nutation_obliquity = nu / 10000;

  var ct = Math.PI / (180 * 3600);
  obliquity = (23 + 26 / 60 + 21.448 / 3600 - 46.815 / 3600 * T_J2000 - 0.00059 / 3600 * T2_J2000 + 0.001813 / 3600 * T3_J2000) * D2R + (nutation_obliquity * ct);

  correct = 22639.55 * Math.sin(n);
  correct += 4586.43061 * Math.sin(2 * d - n);
  correct += 2369.91227 * Math.sin(2 * d);
  correct += 769.02326 * Math.sin(2 * n);
  correct += 211.65487 * Math.sin(2 * d - 2 * n);
  correct += 205.44315 * Math.sin(2 * d - m - n);
  correct += 191.95575 * Math.sin(2 * d + n);
  correct += 164.73458 * Math.sin(2 * d - m);
  correct += 55.17801 * Math.sin(2 * d - 2 * f);
  correct += 39.53393 * Math.sin(n - 2 * f);
  correct += 38.42974 * Math.sin(4 * d - n);
  correct += 36.12364 * Math.sin(3 * n);
  correct += 30.77247 * Math.sin(4 * d - 2 * n);
  correct += 17.95512 * Math.sin(d + m);
  correct += 14.53078 * Math.sin(2 * d - m + n);
  correct += 14.37964 * Math.sin(2 * d + 2 * n);
  correct += 13.89903 * Math.sin(4 * d);
  correct += 13.194 * Math.sin(2 * d - 3 * n);
  correct += 8.60582 * Math.sin(2 * d - m - 2 * n);
  correct += 8.05076 * Math.sin(2 * d - 2 * m);
  correct += 7.37173 * Math.sin(2 * d - 2 * m - n);
  correct += 4.37416 * Math.sin(4 * d - m - n);
  correct += 2.73198 * Math.sin(4 * d - m - 2 * n);
  correct += 2.48897 * Math.sin(2 * d + m - 2 * n);
  correct += 2.14619 * Math.sin(2 * d - m - 2 * f);
  correct += 1.97772 * Math.sin(4 * d + n);
  correct += 1.93367 * Math.sin(4 * n);
  correct += 1.87083 * Math.sin(4 * d - m);
  correct += 1.26186 * Math.sin(d + m + n);
  correct += 1.18682 * Math.sin(4 * d - 3 * n);
  correct += 1.17704 * Math.sin(2 * d - m + 2 * n);
  correct += 1.07773 * Math.sin(d + m - n);
  correct += 1.05949 * Math.sin(2 * d + 3 * n);
  correct += 0.94827 * Math.sin(2 * d - 4 * n);
  correct += 0.75173 * Math.sin(2 * d - 2 * m + n);
  correct += 0.57156 * Math.sin(6 * d - 2 * n);
  correct += 0.47842 * Math.sin(2 * d - m - 3 * n);
  correct += 0.42034 * Math.sin(4 * f);
  correct += 0.41342 * Math.sin(m + 2 * f);
  correct += 0.40423 * Math.sin(3 * d);
  correct += 0.39451 * Math.sin(6 * d - n);
  correct += 0.34966 * Math.sin(d + m - 2 * n);
  correct += 0.33983 * Math.sin(2 * d - 3 * m);
  correct += 0.30874 * Math.sin(4 * d - 2 * m - n);
  correct += 0.30157 * Math.sin(m - n - 2 * f);
  correct += 0.30086 * Math.sin(4 * d - n - 2 * f);
  correct += 0.29422 * Math.sin(2 * d - 2 * m - 2 * n);
  correct += 0.29255 * Math.sin(6 * d - 3 * n);
  correct += 0.28251 * Math.sin(4 * d - m + n);
  correct += 0.27377 * Math.sin(3 * d + m - n);
  correct += 0.26338 * Math.sin(m + n + 2 * f);
  correct += 0.25429 * Math.sin(d + 2 * f);
  correct += 0.24697 * Math.sin(2 * d - 3 * m - n);
  correct += 0.21853 * Math.sin(4 * d + 2 * n);
  correct += 0.17903 * Math.sin(2 * d - n - 2 * f);
  correct += 0.17624 * Math.sin(2 * d + m - 3 * n);
  correct += 0.15781 * Math.sin(4 * d - 2 * m - 2 * n);
  correct += 0.15227 * Math.sin(4 * d - 2 * m);
  correct += 0.1499 * Math.sin(3 * d + m);
  correct += 0.12616 * Math.sin(6 * d);
  correct += 0.111 * Math.sin(5 * n);
  correct += 0.09982 * Math.sin(4 * d - m - 3 * n);
  correct += 0.0932 * Math.sin(2 * d - m + 3 * n);
  correct += 0.09205 * Math.sin(d + m + 2 * n);
  correct += 0.09092 * Math.sin(n + 4 * f);
  correct += 0.09033 * Math.sin(6 * d - m - 2 * n);
  correct += 0.08472 * Math.sin(2 * d + m + n - 2 * f);
  correct += 0.07765 * Math.sin(2 * d + 4 * n);
  correct += 0.07501 * Math.sin(m - 2 * f);
  correct += 0.07142 * Math.sin(6 * d - m - n);
  correct += 0.0685 * Math.sin(2 * d - 5 * n);
  correct += 0.06742 * Math.sin(2 * d + m - n + 2 * f);
  correct += 0.06541 * Math.sin(2 * d + m + 2 * f);
  correct += 0.06507 * Math.sin(3 * d - m);
  correct += 0.06439 * Math.sin(2 * d - 2 * m + 2 * n);
  correct += 0.06314 * Math.sin(2 * d - 2 * m - 2 * f);
  correct += 0.05165 * Math.sin(m - 2 * n - 2 * f);
  correct += 0.0445 * Math.sin(d + n + 2 * f);
  correct += 0.04338 * Math.sin(m + 2 * n + 2 * f);
  correct += 0.04304 * Math.sin(d - 2 * m);
  correct += 0.039 * Math.sin(6* d - m - 3 * n);
  correct += 0.033 * Math.sin(2 * d - 3 * m + n);
  correct += 0.03274 * Math.sin(4 * d - m + 2 * n);
  correct += 0.02949 * Math.sin(2 * d - m - 4 * n);
  correct += 0.02682 * Math.sin(4 * d + m - 3 * n);
  correct += 0.02677 * Math.sin(m + 2 * n - 2 * f);
  correct += 0.0251 * Math.sin(6 * d - m);
  correct += 0.02429 * Math.sin(m - 2 * n + 2 * f);
  correct += 0.02411 * Math.sin(4 * d - 2 * m + n);
  correct += 0.02296 * Math.sin(d + m - 3 * n);
  correct += 0.02289 * Math.sin(4 * d - m - n - 2 * f);
  correct += 0.02285 * Math.sin(6 * d + n);
  correct += 0.02244 * Math.sin(3 * d + m + n);
  correct += 0.02149 * Math.sin(4 * d + 3 * n);
  correct += 0.01993 * Math.sin(2 * d - n + 4 * f);
  correct += 0.01819 * Math.sin(2 * d + m - 4 * n);
  correct += 0.01741 * Math.sin(4 * d - 3 * m - n);
  correct += 0.01605 * Math.sin(2 * d + m + n + 2 * f);
  correct += 0.01598 * Math.sin(d - n + 2 * f);
  correct += 0.01544 * Math.sin(2 * d - 2 * m - 3 * n);
  correct += 0.01376 * Math.sin(6 * d - 4 * n);
  correct += 0.01372 * Math.sin(2 * d + 4 * f);
  correct += 0.01331 * Math.sin(2 * d - 4 * m);
  correct += 0.01297 * Math.sin(2 * n + 4 * f);
  correct += 0.01215 * Math.sin(3 * d - n + 2 * f);
  correct += 0.00971 * Math.sin(4 * d - 3 * m);
  correct += 0.00965 * Math.sin(2 * d - 3 * m - 2 * n);
  correct += 0.00891 * Math.sin(3 * d + m - 2 * f);
  correct += 0.00889 * Math.sin(2 * d + m + 2 * n - 2 * f);
  correct += 0.00866 * Math.sin(8 * d - 2 * n);
  correct += 0.0084 * Math.sin(8 * d - 3 * n);
  correct += 0.00836 * Math.sin(6 * d - 2 * m - 2 * n);
  correct += 0.00812 * Math.sin(2 * d - 4 * m - n);
  correct += 0.00755 * Math.sin(4 * d - 3 * m - 2 * n);
  correct += 0.00744 * Math.sin(6 * d - 2 * m - n);
  correct += 0.0073 * Math.sin(2 * d - m + 4 * n);
  correct += 0.00679 * Math.sin(d + m + 3 * n);
  correct += 0.00666 * Math.sin(4 * d - m - 2 * f);
  correct += 0.00665 * Math.sin(6 * n);
  correct += 0.00662 * Math.sin(4 * d - 2 * n - 2 * f);
  correct += 0.00623 * Math.sin(m - 3 * n - 2 * f);
  correct += 0.00568 * Math.sin(2 * d + 5 * n);
  correct += 0.0056 * Math.sin(4 * d - 2 * m - 3 * n);
  correct += 0.0054 * Math.sin(d + 2 * n + 2 * f);
  correct += 0.00538 * Math.sin(2 * d - 2 * m + 3 * n);
  correct += 0.00526 * Math.sin(m + 3 * n + 2 * f);
  correct += 0.00519 * Math.sin(2 * m + 2 * f);
  correct += 0.00518 * Math.sin(3 * d - 2 * m);
  correct += 0.00515 * Math.sin(2 * d + 2 * m - n + 2 * f);
  correct += 0.00497 * Math.sin(2 * d - 6 * n);
  correct += 0.00477 * Math.sin(6 * d - m + n);
  correct += 0.00475 * Math.sin(5 * d + m - n);
  correct += 0.00473 * Math.sin(2 * m - n - 2 * f);
  correct += 0.00467 * Math.sin(2 * d - 3 * n + 2 * f);
  correct += 0.00455 * Math.sin(8 * d - n);
  correct += 0.00439 * Math.sin(5 * d);
  correct += 0.00392 * Math.sin(5 * d + m - 2 * n);
  correct += 0.00375 * Math.sin(3 * d + 2 * f);
  correct += 0.00364 * Math.sin(6 * d - 2 * n - 2 * f);
  correct += 0.00361 * Math.sin(d + 2 * m - 2 * n);
  correct += 0.00353 * Math.sin(4 * d + m - n + 2 * f);
  correct += 0.00344 * Math.sin(2 * d + n + 4 * f);
  correct += 0.00336 * Math.sin(4 * d - m + 3 * n);
  correct += 0.0033 * Math.sin(3 * d - m + n);
  correct += 0.00324 * Math.sin(8 * d - 4 * n);
  correct += 0.00318 * Math.sin(6 * d + 2 * n);
  correct += 0.00312 * Math.sin(6 * d - 2 * m - 3 * n);
  correct += 0.00298 * Math.sin(3 * d - 2 * n + 2 * f);
  correct += 0.00295 * Math.sin(2 * d - 3 * m + 2 * n);
  correct += 0.0029 * Math.sin(4 * d - 2 * m + 2 * n);
  correct += 0.00289 * Math.sin(d - 2 * n - 2 * f);
  correct += 0.00285 * Math.sin(6 * d - 2 * m);
  correct += 0.00282 * Math.sin(2 * d - 2 * n + 4 * f);
  correct += 0.0027 * Math.sin(2 * m + n + 2 * f);
  correct += 0.00262 * Math.sin(2 * d + m + 2 * n + 2 * f);
  correct += 0.00256 * Math.sin(3 * d + m + 2 * n);
  correct += 0.00254 * Math.sin(d - 3 * m);
  correct += 0.00229 * Math.sin(d - 2 * m - n);
  correct += 0.0022 * Math.sin(4 * d + m - 2 * n + 2 * f);
  correct += 0.00198 * Math.sin(2 * d + m - 4 * f);
  correct += 0.00198 * Math.sin(4 * d + 4 * n);
  correct += 0.00196 * Math.sin(8 * d - m - 2 * n);
  correct += 0.00186 * Math.sin(4 * d + m + 2 * f);
  correct += 0.00183 * Math.sin(4 * d + m + n - 2 * f);
  correct += 0.00181 * Math.sin(5 * d + m);
  correct += 0.00178 * Math.sin(2 * d - m - 5 * n);
  correct += 0.00176 * Math.sin(6 * d - m - 4 * n);
  correct += 0.00173 * Math.sin(2 * d + m - 5 * n);
  correct += 0.0017 * Math.sin(8 * d - m - 3 * n);
  correct += 0.00166 * Math.sin(m + 3 * n - 2 * f);
  correct += 0.00163 * Math.sin(2 * d - 3 * m - 2 * f);
  correct += 0.0016 * Math.sin(4 * d - 3 * m + n);
  correct += 0.00155 * Math.sin(d - m + 2 * f);
  correct += 0.00155 * Math.sin(d + m - 4 * n);
  correct += 0.00153 * Math.sin(3 * n + 4 * f);
  correct += 0.00139 * Math.sin(8 * d);
  correct += 0.00133 * Math.sin(2 * d - 4 * m + n);
  correct += 0.00123 * Math.sin(d - 4 * f);
  correct += 0.00116 * Math.sin(3 * d + m - n - 2 * f);
  correct += 0.00112 * Math.sin(8 * d - m - n);
  correct += 0.00108 * Math.sin(4 * d - 2 * m - n - 2 * f);
  correct += 0.00106 * Math.sin(m - 3 * n + 2 * f);
  correct += 0.00102 * Math.sin(5 * d - m);
  correct += 0.001 * Math.sin(2 * m - 2 * n - 2 * f);
  correct += 0.00096 * Math.sin(2 * d + 2 * m + 2 * f);

  correct -= 666.44186 * Math.sin(m);
  correct -= 411.60287 * Math.sin(2 * f);
  correct -= 147.32654 * Math.sin(m - n);
  correct -= 124.98806 * Math.sin(d);
  correct -= 109.38419 * Math.sin(m + n);
  correct -= 45.10032 * Math.sin(n + 2 * f);
  correct -= 28.3981 * Math.sin(2 * d + m - n);
  correct -= 24.3591 * Math.sin(2 * d + m);
  correct -= 18.58467 * Math.sin(d - n);
  correct -= 9.67938 * Math.sin(m - 2 * n);
  correct -= 9.36601 * Math.sin(2 * d - n + 2 * f);
  correct -= 8.45308 * Math.sin(d + n);
  correct -= 7.63041 * Math.sin(m + 2 * n);
  correct -= 7.44804 * Math.sin(2 * m);
  correct -= 6.38325 * Math.sin(2 * d + n - 2 * f);
  correct -= 5.7417 * Math.sin(2 * d + 2 * f);
  correct -= 3.99767 * Math.sin(2 * n + 2 * f);
  correct -= 3.20968 * Math.sin(3 * d - n);
  correct -= 2.91464 * Math.sin(2 * d + m + n);
  correct -= 2.56813 * Math.sin(2 * m - n);
  correct -= 2.52138 * Math.sin(2 * d + 2 * m - n);
  correct -= 1.75296 * Math.sin(d - 2 * n);
  correct -= 1.43724 * Math.sin(2 * d + m - 2 * f);
  correct -= 1.37259 * Math.sin(2 * n - 2 * f);
  correct -= 1.22412 * Math.sin(3 * d - 2 * n);
  correct -= 1.16177 * Math.sin(2 * m + n);
  correct -= 0.99023 * Math.sin(2 * d + n + 2 * f);
  correct -= 0.6694 * Math.sin(m - 3 * n);
  correct -= 0.63523 * Math.sin(4 * d + m - n);
  correct -= 0.58399 * Math.sin(d + 2 * n);
  correct -= 0.58332 * Math.sin(d - 2 * f);
  correct -= 0.56065 * Math.sin(2 * d - 2 * n - 2 * f);
  correct -= 0.55694 * Math.sin(d - m);
  correct -= 0.54594 * Math.sin(m + 3 * n);
  correct -= 0.53572 * Math.sin(2* d - 2 * n + 2 * f);
  correct -= 0.4538 * Math.sin(2 * d + 2 * n - 2 * f);
  correct -= 0.42624 * Math.sin(2 * d - m - n + 2 * f);
  correct -= 0.38215 * Math.sin(2 * d - m + 2 * f);
  correct -= 0.37453 * Math.sin(2 * d - m + n - 2 * f);
  correct -= 0.35759 * Math.sin(4 * d + m - 2 * n);
  correct -= 0.32866 * Math.sin(3 * n + 2 * f);
  correct -= 0.29023 * Math.sin(2 * d + m + 2 * n);
  correct -= 0.28911 * Math.sin(4 * d + m);
  correct -= 0.25304 * Math.sin(3 * d - 2 * f);
  correct -= 0.2499 * Math.sin(2 * d + 2 * m - 2 * n);
  correct -= 0.23141 * Math.sin(3 * d - m - n);
  correct -= 0.20134 * Math.sin(4 * d - n + 2 * f);
  correct -= 0.19311 * Math.sin(2 * m - 2 * n);
  correct -= 0.18576 * Math.sin(2 * d + 2 * m);
  correct -= 0.16977 * Math.sin(4 * d - 2 * n + 2 * f);
  correct -= 0.13636 * Math.sin(d - m - n);
  correct -= 0.12812 * Math.sin(d - 3 * n);
  correct -= 0.12386 * Math.sin(2 * d + 2 * n + 2 * f);
  correct -= 0.12073 * Math.sin(d - m + n);
  correct -= 0.10136 * Math.sin(3 * m);
  correct -= 0.09154 * Math.sin(2 * d - 3 * n - 2 * f);
  correct -= 0.085 * Math.sin(4 * d + 2 * f);
  correct -= 0.08311 * Math.sin(3 * d - m - 2 * n);
  correct -= 0.08282 * Math.sin(m + n - 2 * f);
  correct -= 0.08049 * Math.sin(m - n + 2 * f);
  correct -= 0.08019 * Math.sin(n - 4 * f);
  correct -= 0.07518 * Math.sin(2 * d - 4 * f);
  correct -= 0.07373 * Math.sin(2 * d - m + n + 2 * f);
  correct -= 0.06601 * Math.sin(4 * d + n - 2 * f);
  correct -= 0.06513 * Math.sin(2 * m + 2 * n);
  correct -= 0.06103 * Math.sin(2 * d - m - n - 2 * f);
  correct -= 0.05725 * Math.sin(5 * d - 2 * n);
  correct -= 0.05684 * Math.sin(3 * n - 2 * f);
  correct -= 0.05142 * Math.sin(3 * m - n);
  correct -= 0.0507 * Math.sin(4 * d + m + n);
  correct -= 0.04702 * Math.sin(m - 4 * n);
  correct -= 0.04442 * Math.sin(3 * d - 3 * n);
  correct -= 0.04189 * Math.sin(3 * d + m - 2 * n);
  correct -= 0.04074 * Math.sin(d + 3 * n);
  correct -= 0.04012 * Math.sin(d + n - 2 * f);
  correct -= 0.03968 * Math.sin(d + 2 * m);
  correct -= 0.03947 * Math.sin(m + 4 * n);
  correct -= 0.03587 * Math.sin(d + m + 2 * f);
  correct -= 0.03514 * Math.sin(4 * d + 2 * m - 2 * n);
  correct -= 0.03336 * Math.sin(2 * d + 3 * n - 2 * f);
  correct -= 0.02979 * Math.sin(3 * d - n - 2 * f);
  correct -= 0.02887 * Math.sin(2 * d - m + 2 * n - 2 * f);
  correct -= 0.02804 * Math.sin(2 * d - m - 2 * n - 2 * f);
  correct -= 0.02676 * Math.sin(2 * d + m + 3 * n);
  correct -= 0.02602 * Math.sin(4 * n + 2 * f);
  correct -= 0.02391 * Math.sin(4 * d - 2 * f);
  correct -= 0.02379 * Math.sin(d - n - 2 * f);
  correct -= 0.02349 * Math.sin(2 * d + 2 * m - 2 * f);
  correct -= 0.02273 * Math.sin(4 * d - m - n + 2 * f);
  correct -= 0.02171 * Math.sin(4 * d + 2 * m - n);
  correct -= 0.02157 * Math.sin(2 * d - m - 2 * n + 2 * f);
  correct -= 0.01948 * Math.sin(3 * d - m - 2 * f);
  correct -= 0.01875 * Math.sin(4 * d + m - n - 2 * f);
  correct -= 0.01816 * Math.sin(2 * d - 2 * m + 2 * f);
  correct -= 0.01796 * Math.sin(3 * m + n);
  correct -= 0.01781 * Math.sin(4 * d + n + 2 * f);
  correct -= 0.01686 * Math.sin(5 * d - 3 * n);
  correct -= 0.01644 * Math.sin(2 * d - 2 * m + n - 2 * f);
  correct -= 0.01541 * Math.sin(2 * d - 2 * m - n + 2 * f);
  correct -= 0.01533 * Math.sin(4 * d - m - 2 * n + 2 * f);
  correct -= 0.01514 * Math.sin(2 * m - 3 * n);
  correct -= 0.01483 * Math.sin(d - m + 2 * n);
  correct -= 0.0135 * Math.sin(5 * d - n);
  correct -= 0.01343 * Math.sin(2 * d + 2 * m + n);
  correct -= 0.01332 * Math.sin(2 * d + 3 * n + 2 * f);
  correct -= 0.01282 * Math.sin(6 * d + m - 2 * n);
  correct -= 0.01281 * Math.sin(d - m - 2 * f);
  correct -= 0.01182 * Math.sin(3 * d - 2 * m - n);
  correct -= 0.01114 * Math.sin(4 * d - m + 2 * f);
  correct -= 0.01077 * Math.sin(2 * d - 4 * n - 2 * f);
  correct -= 0.01064 * Math.sin(6 * d + m - n);
  correct -= 0.01062 * Math.sin(3 * d + n - 2 * f);
  correct -= 0.01007 * Math.sin(2 * d - m + 2 * n + 2 * f);
  correct -= 0.0098 * Math.sin(4 * d + 2 * n - 2 * f);
  correct -= 0.00955 * Math.sin(d - 4 * n);
  correct -= 0.00944 * Math.sin(2 * d + 2 * m - 3 * n);
  correct -= 0.00934 * Math.sin(4 * d - 3 * n + 2 * f);
  correct -= 0.0085 * Math.sin(2 * d - n - 4 * f);
  correct -= 0.00849 * Math.sin(d + 2 * m + n);
  correct -= 0.00732 * Math.sin(4 * d - m + n - 2 * f);
  correct -= 0.00694 * Math.sin(d - m - 2 * n);
  correct -= 0.00693 * Math.sin(5 * d - m - 2 * n);
  correct -= 0.00668 * Math.sin(4 * d + m + 2 * n);
  correct -= 0.00659 * Math.sin(d + m + n + 2 * f);
  correct -= 0.00654 * Math.sin(2 * d + 2 * m - n - 2 * f);
  correct -= 0.00623 * Math.sin(3 * d + m - 3 * n);
  correct -= 0.00509 * Math.sin(6 * d - 2 * n + 2 * f);
  correct -= 0.00478 * Math.sin(6 * d + m - 3 * n);
  correct -= 0.00434 * Math.sin(2 * d - 2 * m - n - 2 * f);
  correct -= 0.00431 * Math.sin(4 * d - 5 * n);
  correct -= 0.00416 * Math.sin(3 * m - 2 * n);
  correct -= 0.00399 * Math.sin(3 * d - 2 * m - 2 * n);
  correct -= 0.00396 * Math.sin(6 * d + m);
  correct -= 0.00389 * Math.sin(3 * d + 2 * n);
  correct -= 0.00378 * Math.sin(2 * d - 2 * m + n + 2 * f);
  correct -= 0.00369 * Math.sin(4 * d + 2 * m - 3 * n);
  correct -= 0.00365 * Math.sin(2 * d - m - 3 * n - 2 * f);
  correct -= 0.00359 * Math.sin(6 * d - n + 2 * f);
  correct -= 0.00355 * Math.sin(2 * m - 2 * f);
  correct -= 0.00354 * Math.sin(4 * n - 2 * f);
  correct -= 0.00346 * Math.sin(2 * d + m - 2 * n - 2 * f);
  correct -= 0.00341 * Math.sin(2 * m + 3 * n);
  correct -= 0.00335 * Math.sin(5 * d - n - 2 * f);
  correct -= 0.00332 * Math.sin(m - 5 * n);
  correct -= 0.003 * Math.sin( d + 2 * m - n);
  correct -= 0.00297 * Math.sin(3 * d - m - 3 * n);
  correct -= 0.00287 * Math.sin(m + 5 * n);
  correct -= 0.00287 * Math.sin(6 * d - 3 * n + 2 * f);
  correct -= 0.00286 * Math.sin(2 * d - m - 4 * f);
  correct -= 0.00285 * Math.sin(d + 4 * n);
  correct -= 0.00274 * Math.sin(4 * d + 2 * n + 2 * f);
  correct -= 0.00251 * Math.sin(4 * d - m + n + 2 * f);
  correct -= 0.00247 * Math.sin(2 * d + 4 * n - 2 * f);
  correct -= 0.00236 * Math.sin(2 * d + m + 4 * n);
  correct -= 0.00232 * Math.sin(2 * d - m + 3 * n - 2 * f);
  correct -= 0.00228 * Math.sin(2 * d + m - n - 2 * f);
  correct -= 0.00214 * Math.sin(6 * d - 2 * f);
  correct -= 0.00212 * Math.sin(d - m + n - 2 * f);
  correct -= 0.00208 * Math.sin(4 * d + 2 * m);
  correct -= 0.00201 * Math.sin(5 * n + 2 * f);
  correct -= 0.002 * Math.sin(2 * d + 2 * m + n - 2 * f);
  correct -= 0.00191 * Math.sin(3 * d + 2 * m);
  correct -= 0.00189 * Math.sin(3 * d - m - n - 2 * f);
  correct -= 0.00189 * Math.sin(5 * d - m - 3 * n);
  correct -= 0.00188 * Math.sin(2 * d + 3 * m - n);
  correct -= 0.00174 * Math.sin(3 * d - 4 * n);
  correct -= 0.0016 * Math.sin(4 * d - 2 * m - n + 2 * f);
  correct -= 0.00157 * Math.sin(d + m + n - 2 * f);
  correct -= 0.00154 * Math.sin(5 * d - m - n);
  correct -= 0.00149 * Math.sin(d - m + 3 * n);
  correct -= 0.00142 * Math.sin(d - 2 * n + 2 * f);
  correct -= 0.00138 * Math.sin(3 * d + m - n + 2 * f);
  correct -= 0.00137 * Math.sin(5 * d - 2 * f);
  correct -= 0.00133 * Math.sin(2 * d - 2 * m + 2 * n - 2 * f);
  correct -= 0.00132 * Math.sin(6 * d + 2 * f);
  correct -= 0.00131 * Math.sin(2 * d + 4 * n + 2 * f);
  correct -= 0.00128 * Math.sin(4 * m);
  correct -= 0.00127 * Math.sin(3 * d + 2 * m - n);
  correct -= 0.00121 * Math.sin(4 * d - m + 2 * n - 2 * f);
  correct -= 0.00119 * Math.sin(2 * m - 4 * n);
  correct -= 0.00117 * Math.sin(2 * d - m + 3 * n + 2 * f);
  correct -= 0.00116 * Math.sin(2 * d + m - 3 * n - 2 * f);
  correct -= 0.00111 * Math.sin(2 * d - 2 * m - 2 * n - 2 * f);
  correct -= 0.00111 * Math.sin(2 * d - 5 * n - 2 * f);
  correct -= 0.00109 * Math.sin(4 * d + 3 * n - 2 * f);
  correct -= 0.00108 * Math.sin(4 * m - n);
  correct -= 0.00102 * Math.sin(d + 2 * m + 2 * n);
  correct -= 0.00102 * Math.sin(3 * d - 2 * m - 2 * f);
  correct -= 0.001 * Math.sin(d - m - n - 2 * f);
  correct -= 0.00098 * Math.sin(7 * d - 3 * n);

  correct += 14.2488 * Math.sin(18 * venus - 16 * mean_longitude_earth - n + 26.54261 * D2R);
  correct += 1.1431 * Math.sin(2 * mean_longitude_earth - 2 * jupiter + 2 * d - n + 180.11977 * D2R);
  correct += 0.9011 * Math.sin(4 * mean_longitude_earth - 8 * mars + 3 * jupiter + 285.98707 * D2R);
  correct += 0.8216 * Math.sin(venus - mean_longitude_earth + 180.00988 * D2R);
  correct += 0.7881 * Math.sin(18 * venus - 16 * mean_longitude_earth - 2 * n + 26.54324 * D2R);
  correct += 0.7393 * Math.sin(18 * venus - 16 * mean_longitude_earth + 26.54560 * D2R);
  correct += 0.6437 * Math.sin(3 * venus - 3 * mean_longitude_earth + 2 * d - n + 179.98144 * D2R);
  correct += 0.6388 * Math.sin(mean_longitude_earth - jupiter + 1.22890 * D2R);
  correct += 0.5634 * Math.sin(10 * venus - 3 * mean_longitude_earth - n + 333.30551 * D2R);
  correct += 0.4453 * Math.sin(2 * mean_longitude_earth - 3 * jupiter + 2 * d - n + 10.07001 * D2R);
  correct += 0.3436 * Math.sin(2 * venus - 3 * mean_longitude_earth + 269.95393 * D2R);
  correct += 0.3246 * Math.sin(mean_longitude_earth - 2 * mars + 318.13776 * D2R);
  correct += 0.3016 * Math.sin(2 * venus - 2 * mean_longitude_earth + 0.20448 * D2R);

  correct += 7.06304 * Math.sin(dzeta_moon - f + 0.00094 * D2R);
  correct += 0.49331 * Math.sin(dzeta_moon + small_l - f + 0.00127 * D2R);
  correct += 0.49141 * Math.sin(dzeta_moon - small_l - f + 0.00127 * D2R);
  correct += 0.36061 * Math.sin(dzeta_moon + f + 0.00071 * D2R);
  correct += 0.09642 * Math.sin(dzeta_moon + 2 * d - f + 0.0009 * D2R);
  correct += 0.06569 * Math.sin(dzeta_moon - 2 * d - f + 0.001 * D2R);
  correct += 0.06456 * Math.sin(dzeta_moon + 2 * d - small_l - f + 0.00042 * D2R);
  correct += 0.05036 * Math.sin(dzeta_moon - small_l + f + 0.00051 * D2R);
  correct += 0.04962 * Math.sin(dzeta_moon - 2 * d + small_l - f + 0.00029 * D2R);
  correct += 0.04746 * Math.sin(dzeta_moon - 2 * d + f + 0.00076 * D2R);
  correct += 0.03838 * Math.sin(dzeta_moon + small_l + f + 0.0007 * D2R);
  correct += 0.03638 * Math.sin(2 * dzeta_moon - 2 * f + 180 * D2R);
  correct += 0.03402 * Math.sin(dzeta_moon + 2 * small_l - f + 0.00126 * D2R);
  correct += 0.03279 * Math.sin(dzeta_moon - 2 * small_l - f + 0.00128 * D2R);
  correct += 0.02206 * Math.sin(2 * d - small_l);
  correct += 0.01492 * Math.sin(dzeta_moon - 3 * f + 180.00086 * D2R);
  correct += 0.01234 * Math.sin(dzeta_moon + 2 * d + small_l - f + 0.00102 * D2R);

  l += (correct / 3600 * D2R) + (nutation_longitude * ct);
  longitude_moon = l;
  l1 = longitude_moon * R2D;

  z1 = ((l1 - Math.floor(l1)) * 60);
  z2 = ((z1 - Math.floor(z1)) * 60);

  document.getElementById("longmoon").innerHTML = Math.floor(l1) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';

  correct = 18461.4 * Math.sin(f);
  correct -= 6.29664 * Math.sin(3 * f);
  correct += 2.79871 * Math.sin(n - 3 * f);
  correct += 999.70079 * Math.sin(n - f);
  correct += 1010.1743 * Math.sin(n + f);
  correct -= 1.01941 * Math.sin(n + 3 * f);
  correct -= 0.13035 * Math.sin(2 * n - 3 * f);
  correct += 31.75985 * Math.sin(2 * n - f);
  correct += 61.91229 * Math.sin(2 * n + f);
  correct -= 0.11787 * Math.sin(2 * n + 3 * f);
  correct += 1.58131 * Math.sin(3 * n - f);
  correct += 3.98407 * Math.sin(3 * n + f);
  correct -= 0.01181 * Math.sin(3 * n + 3 * f);
  correct += 0.09157 * Math.sin(4 * n - f);
  correct += 0.26325 * Math.sin(4 * n + f);
  correct += 0.01768 * Math.sin(5 * n + f);
  correct -= 0.07479 * Math.sin(m - 3 * n - f);
  correct -= 0.02365 * Math.sin(m - 3 * n + f);
  correct -= 0.79322 * Math.sin(m - 2 * n - f);
  correct -= 0.30129 * Math.sin(m - 2 * n + f);
  correct -= 6.73173 * Math.sin(m - n - f);
  correct -= 5.6326 * Math.sin(m - n + f);
  correct -= 4.83983 * Math.sin(m - f);
  correct -= 6.46036 * Math.sin(m + f);
  correct += 0.01157 * Math.sin(m + 3 * f);
  correct -= 5.07614 * Math.sin(m + n - f);
  correct -= 5.31151 * Math.sin(m + n + f);
  correct -= 0.31292 * Math.sin(m + 2 * n - f);
  correct -= 0.63884 * Math.sin(m + 2 * n + f);
  correct -= 0.02419 * Math.sin(m + 3 * n - f);
  correct -= 0.06176 * Math.sin(m + 3 * n + f);
  correct -= 0.01571 * Math.sin(2 * m - 2 * n - f);
  correct -= 0.11335 * Math.sin(2 * m - n - f);
  correct -= 0.09511 * Math.sin(2 * m - n + f);
  correct -= 0.01801 * Math.sin(2 * m - f);
  correct -= 0.05729 * Math.sin(2 * m + f);
  correct -= 0.06187 * Math.sin(2 * m + n - f);
  correct -= 0.05504 * Math.sin(2 * m + n + f);
  correct += 0.01031 * Math.sin(d - m - n - f);
  correct -= 0.01346 * Math.sin(d - m - f);
  correct -= 0.01829 * Math.sin(d - m + f);
  correct -= 0.02012 * Math.sin(d - m + n - f);
  correct -= 0.01255 * Math.sin(d - 3 * n - f);
  correct -= 0.10964 * Math.sin(d - 2 * n - f);
  correct -= 0.07846 * Math.sin(d - 2 * n + f);
  correct -= 0.42989 * Math.sin(d - n - f);
  correct += 0.13928 * Math.sin(d - n + f);
  correct -= 0.03226 * Math.sin(d - 3 * f);
  correct -= 4.80578 * Math.sin(d - f);
  correct -= 5.36844 * Math.sin(d + f);
  correct -= 0.58893 * Math.sin(d + n - f);
  correct -= 0.66741 * Math.sin(d + n + f);
  correct -= 0.03636 * Math.sin(d + 2 * n - f);
  correct -= 0.06383 * Math.sin(d + 2 * n + f);
  correct += 0.01597 * Math.sin(d + m - 2 * n - f);
  correct += 0.0168 * Math.sin(d + m - 2 * n + f);
  correct -= 0.0559 * Math.sin(d + m - n + f);
  correct += 0.80426 * Math.sin(d + m - f);
  correct += 0.80263 * Math.sin(d + m + f);
  correct += 0.03465 * Math.sin(d + m + n - f);
  correct += 0.10176 * Math.sin(d + m + n + f);
  correct += 0.01016 * Math.sin(d + m + 2 * n + f);
  correct += 0.01042 * Math.sin(2 * d - 3 * m - n + f);
  correct += 0.03647 * Math.sin(2 * d - 3 * m - f);
  correct += 0.01603 * Math.sin(2 * d - 3 * m + f);
  correct += 0.02285 * Math.sin(2 * d - 2 * m - 2 * n - f);
  correct += 0.26865 * Math.sin(2 * d - 2 * m - n - f);
  correct += 0.31474 * Math.sin(2 * d - 2 * m - n + f);
  correct += 1.08587 * Math.sin(2 * d - 2 * m - f);
  correct += 0.38353 * Math.sin(2 * d - 2 * m + f);
  correct += 0.06915 * Math.sin(2 * d - 2 * m + n - f);
  correct += 0.05848 * Math.sin(2 * d - 2 * m + n + f);
  correct += 0.05502 * Math.sin(2 * d - m - 3 * n - f);
  correct += 0.65025 * Math.sin(2 * d - m - 2 * n - f);
  correct -= 0.06208 * Math.sin(2 * d - m - 2 * n + f);
  correct += 0.01034 * Math.sin(2 * d - m - n - 3 * f);
  correct += 7.43488 * Math.sin(2 * d - m - n - f);
  correct += 8.86853 * Math.sin(2 * d - m - n + f);
  correct -= 0.01177 * Math.sin(2 * d - m - n + 3 * f);
  correct += 0.08815 * Math.sin(2 * d - m - 3 * f);
  correct += 29.57794 * Math.sin(2 * d - m - f);
  correct += 7.95891 * Math.sin(2 * d - m + f);
  correct -= 0.01669 * Math.sin(2 * d - m + n - 3 * f);
  correct += 1.76606 * Math.sin(2 * d - m + n - f);
  correct += 1.13466 * Math.sin(2 * d - m + n + f);
  correct += 0.12897 * Math.sin(2 * d - m + 2 * n - f);
  correct += 0.12387 * Math.sin(2 * d - m + 2 * n + f);
  correct += 0.01211 * Math.sin(2 * d - m + 3 * n + f);
  correct += 0.01127 * Math.sin(2 * d - 5 * n - f);
  correct += 0.13381 * Math.sin(2 * d - 4 * n - f);
  correct += 0.02496 * Math.sin(2 * d - 4 * n + f);
  correct += 1.51564 * Math.sin(2 * d - 3 * n - f);
  correct += 0.25408 * Math.sin(2 * d - 3 * n + f);
  correct += 0.02045 * Math.sin(2 * d - 2 * n - 3 * f);
  correct += 15.56635 * Math.sin(2 * d - 2 * n - f);
  correct -= 1.62443 * Math.sin(2 * d - 2 * n + f);
  correct -= 0.06561 * Math.sin(2 * d - 2 * n + 3 * f);
  correct += 0.32907 * Math.sin(2 * d - n - 3 * f);
  correct += 166.57528 * Math.sin(2 * d - n - f);
  correct += 199.48515 * Math.sin(2 * d - n + f);
  correct -= 0.24484 * Math.sin(2 * d - n + 3 * f);
  correct += 2.18637 * Math.sin(2 * d - 3 * f);
  correct += 623.65783 * Math.sin(2 * d - f);
  correct += 117.26161 * Math.sin(2 * d + f);
  correct -= 0.14453 * Math.sin(2 * d + 3 * f);
  correct -= 0.29116 * Math.sin(2 * d + n - 3 * f);
  correct += 33.35743 * Math.sin(2 * d + n - f);
  correct += 15.12165 * Math.sin(2 * d + n + f);
  correct -= 0.03038 * Math.sin(2 * d + n + 3 * f);
  correct += 2.14618 * Math.sin(2 * d + 2 * n - f);
  correct += 1.51976 * Math.sin(2 * d + 2 * n + f);
  correct += 0.14642 * Math.sin(2 * d + 3 * n - f);
  correct += 0.13795 * Math.sin(2 * d + 3 * n + f);
  correct += 0.01027 * Math.sin(2 * d + 4 * n - f);
  correct += 0.01186 * Math.sin(2 * d + 4 * n + f);
  correct += 0.01818 * Math.sin(2 * d + m - 3 * n - f);
  correct += 0.07913 * Math.sin(2 * d + m - 2 * n - f);
  correct += 0.05429 * Math.sin(2 * d + m - 2 * n + f);
  correct -= 0.79105 * Math.sin(2 * d + m - n - f);
  correct -= 1.31788 * Math.sin(2 * d + m - n + f);
  correct -= 0.05457 * Math.sin(2 * d + m - 3 * f);
  correct -= 12.0947 * Math.sin(2 * d + m - f);
  correct -= 1.26433 * Math.sin(2 * d + m + f);
  correct -= 0.82275 * Math.sin(2 * d + m + n - f);
  correct -= 0.23702 * Math.sin(2 * d + m + n + f);
  correct -= 0.06283 * Math.sin(2 * d + m + 2 * n - f);
  correct -= 0.03142 * Math.sin(2 * d + m + 2 * n + f);
  correct -= 0.01262 * Math.sin(2 * d + 2 * m - 2 * n - f);
  correct -= 0.10535 * Math.sin(2 * d + 2 * m - n - f);
  correct -= 0.1133 * Math.sin(2 * d + 2 * m - n + f);
  correct -= 0.13415 * Math.sin(2 * d + 2 * m - f);
  correct -= 0.01482 * Math.sin(2 * d + 2 * m + f);
  correct -= 0.02104 * Math.sin(3 * d - m - n - f);
  correct -= 0.01356 * Math.sin(3 * d - m - n + f);
  correct -= 0.02572 * Math.sin(3 * d - m - f);
  correct -= 0.03941 * Math.sin(3 * d - 2 * n - f);
  correct -= 0.04852 * Math.sin(3 * d - 2 * n + f);
  correct -= 0.30517 * Math.sin(3 * d - n - f);
  correct -= 0.20593 * Math.sin(3 * d - n + f);
  correct -= 0.01009 * Math.sin(3 * d - 3 * f);
  correct -= 0.35183 * Math.sin(3 * d - f);
  correct -= 0.0284 * Math.sin(3 * d + f);
  correct -= 0.03611 * Math.sin(3 * d + n - f);
  correct += 0.01321 * Math.sin(3 * d + m - n - f);
  correct += 0.02083 * Math.sin(3 * d + m - n + f);
  correct += 0.03436 * Math.sin(3 * d + m - f);
  correct += 0.01351 * Math.sin(3 * d + m + f);
  correct += 0.0123 * Math.sin(4 * d - 2 * m - 2 * n + f);
  correct += 0.03462 * Math.sin(4 * d - 2 * m - n - f);
  correct += 0.0238 * Math.sin(4 * d - 2 * m - n + f);
  correct += 0.02899 * Math.sin(4 * d - 2 * m - f);
  correct += 0.0127 * Math.sin(4 * d - 2 * m + f);
  correct += 0.05251 * Math.sin(4 * d - m - 2 * n - f);
  correct += 0.21376 * Math.sin(4 * d - m - 2 * n + f);
  correct += 0.5958 * Math.sin(4 * d - m - n - f);
  correct += 0.33882 * Math.sin(4 * d - m - n + f);
  correct += 0.41496 * Math.sin(4 * d - m - f);
  correct += 0.15791 * Math.sin(4 * d - m + f);
  correct += 0.05686 * Math.sin(4 * d - m + n - f);
  correct += 0.03009 * Math.sin(4 * d - m + n + f);
  correct += 0.02174 * Math.sin(4 * d - 3 * n + f);
  correct += 0.63371 * Math.sin(4 * d - 2 * n - f);
  correct += 2.41389 * Math.sin(4 * d - 2 * n + f);
  correct += 6.57962 * Math.sin(4 * d - n - f);
  correct += 2.9985 * Math.sin(4 * d - n + f);
  correct += 0.06257 * Math.sin(4 * d - 3 * f);
  correct += 3.67449 * Math.sin(4 * d - f);
  correct += 1.19188 * Math.sin(4 * d + f);
  correct += 0.47338 * Math.sin(4 * d + n - f);
  correct += 0.21259 * Math.sin(4 * d + n + f);
  correct += 0.04834 * Math.sin(4 * d + 2 * n - f);
  correct += 0.02828 * Math.sin(4 * d + 2 * n + f);
  correct -= 0.02957 * Math.sin(4 * d + m - 2 * n + f);
  correct -= 0.17191 * Math.sin(4 * d + m - n - f);
  correct -= 0.05097 * Math.sin(4 * d + m - n + f);
  correct -= 0.11308 * Math.sin(4 * d + m - f);
  correct -= 0.02549 * Math.sin(4 * d + m + f);
  correct -= 0.01692 * Math.sin(4 * d + m + n - f);
  correct -= 0.01049 * Math.sin(5 * d - n - f);
  correct += 0.01091 * Math.sin(6 * d - m - 2 * n - f);
  correct += 0.01486 * Math.sin(6 * d - m - n - f);
  correct += 0.03118 * Math.sin(6 * d - 3 * n + f);
  correct += 0.08096 * Math.sin(6 * d - 2 * n - f);
  correct += 0.05963 * Math.sin(6 * d - 2 * n + f);
  correct += 0.09403 * Math.sin(6 * d - n - f);
  correct += 0.04217 * Math.sin(6 * d - n + f);
  correct += 0.03674 * Math.sin(6 * d - f);
  correct += 0.01465 * Math.sin(6 * d + f);

  correct += 8.045 * Math.sin(dzeta_moon + 180 * D2R);
  correct += 0.416 * Math.sin(dzeta_moon + small_l + 180 * D2R);
  correct += 0.456 * Math.sin(dzeta_moon - small_l);
  correct += 0.326 * Math.sin(dzeta_moon - 2 * f);

  correct += 0.63 * Math.sin(18 * venus - 16 * mean_longitude_earth - small_l + f + 26.54 * D2R);
  correct += 0.63 * Math.sin(18 * venus - 16 * mean_longitude_earth - small_l - f + 26.54 * D2R);
  correct += 0.14 * Math.sin(mean_longitude_earth + d + 291.98 * D2R);
  correct += 0.07 * Math.sin(18 * venus - 16 * mean_longitude_earth - 2 * small_l - f + 26.54 * D2R);
  correct += 0.067 * Math.sin(18 * venus - 16 * mean_longitude_earth + f + 26.54 * D2R);
  correct += 0.067 * Math.sin(5 * venus - 6 * mean_longitude_earth + 2 * d - f + 272.3 * D2R);

  correct += 1.375 * Math.sin(mean_longitude_earth + d + 275.13 * D2R);
  correct += 0.078 * Math.sin(mean_longitude_earth + d - small_l + 95.13 * D2R);

  correct -= 0.00001754 * 3600 * D2R * Math.sin((183.3 + (483202 * T_J2000)) * D2R);

  latitude_moon = correct / 3600 * D2R;

  b1 = Math.abs(latitude_moon * R2D);
  z1 = ((b1 - Math.floor(b1)) * 60);
  z2 = ((z1 - Math.floor(z1)) * 60);

  if ( latitude_moon < 0 )
    sign = "-";
  else
    sign = "&nbsp;";

  document.getElementById("latmoon").innerHTML = sign + Math.floor(b1) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';

  p = 385000.56 - 3.14837 * Math.cos(2 * f) + 79.66183 * Math.cos(n - 2 * f);
  p -= 20905.32206 * Math.cos(n) + 0.10326 * Math.cos(n + 2 * f);
  p -= 4.42124 * Math.cos(2 * n - 2 * f) + 569.92332 * Math.cos(2 * n);
  p -= 23.21032 * Math.cos(3 * n) + 1.11693 * Math.cos(4 * n);
  p -= 0.42242 * Math.cos(m - 3 * n) + 7.00293 * Math.cos(m - 2 * n);
  p -= 129.62476 * Math.cos(m - n) - 0.33465 * Math.cos(m - n + 2 * f);
  p -= 0.18568 * Math.cos(m - 2 * f) - 48.89 * Math.cos(m);
  p -= 0.15803 * Math.cos(m + 2 * f) + 0.2481 * Math.cos(m + n - 2 * f);
  p += 104.75896 * Math.cos(m + n) + 5.75105 * Math.cos(m + 2 * n);
  p += 0.35509 * Math.cos(m + 3 * n) - 0.13618 * Math.cos(2 * m - 2 * n);
  p -= 2.11728 * Math.cos(2 * m - n) - 1.06575 * Math.cos(2 * m);
  p += 1.16562 * Math.cos(2 * m + n) + 0.1141 * Math.cos(d - m - n);
  p += 0.49757 * Math.cos(d - m) + 0.10998 * Math.cos(d - m + n);
  p -= 1.73852 * Math.cos(d - 2 * n) + 8.37909 * Math.cos(d - n);
  p -= 0.79564 * Math.cos(d - 2 * f) - 108.74265 * Math.cos(d);
  p += 6.32199 * Math.cos(d + n) + 0.37852 * Math.cos(d + 2 * n);
  p += 0.33226 * Math.cos(d + m - 2 * n) + 0.85127 * Math.cos(d + m - n);
  p -= 16.67533 * Math.cos(d + m) + 0.93335 * Math.cos(d + m + n);
  p -= 0.14808 * Math.cos(2 * d - 3 * m - n) + 0.41076 * Math.cos(2 * d - 3 * m);
  p += 0.34304 * Math.cos(2 * d - 2 * m - 2 * n) - 4.95049 * Math.cos(2 * d - 2 * m - n);
  p -= 9.88519 * Math.cos(2 * d - 2 * m) + 0.65758 * Math.cos(2 * d - 2 * m + n);
  p += 0.49506 * Math.cos(2 * d - m - 3 * n) + 10.05654 * Math.cos(2 * d - m - 2 * n);
  p += 0.32336 * Math.cos(2 * d - m - n - 2 * f) - 152.14314 * Math.cos(2 * d - m - n);
  p += 0.657 * Math.cos(2 * d - m - 2 * f) - 204.59357 * Math.cos(2 * d - m);
  p += 0.20942 * Math.cos(2 * d - m + n - 2 * f) - 12.83185 * Math.cos(2 * d - m + n);
  p -= 0.84883 * Math.cos(2 * d - m + 2 * n) - 0.77854 * Math.cos(2 * d - 4 * n);
  p += 14.40262 * Math.cos(2 * d - 3 * n) + 0.47263 * Math.cos(2 * d - 2 * n - 2 * f);
  p += 246.15768 * Math.cos(2 * d - 2 * n) + 0.77405 * Math.cos(2 * d - 2 * n + 2 * f);
  p += 8.7517 * Math.cos(2 * d - n - 2 * f) - 3699.10468 * Math.cos(2 * d - n);
  p += 0.59633 * Math.cos(2 * d - n + 2 * f) + 10.32129 * Math.cos(2 * d - 2 * f);
  p -= 2955.9665 * Math.cos(2 * d) - 4.13118 * Math.cos(2 * d + n - 2 * f);
  p -= 170.73274 * Math.cos(2 * d + n) - 0.28399 * Math.cos(2 * d + 2 * n - 2 * f);
  p -= 10.44472 * Math.cos(2 * d + 2 * n) + 0.66968 * Math.cos(2 * d + 3 * n);
  p += 0.16858 * Math.cos(2 * d + m - 3 * n) + 0.14368 * Math.cos(2 * d + m - 2 * n);
  p += 24.20935 * Math.cos(2 * d + m - n) - 0.13572 * Math.cos(2 * d + m - 2 * f);
  p += 30.82498 * Math.cos(2 * d + m) + 2.6165 * Math.cos(2 * d + m + n);
  p += 0.21252 * Math.cos(2 * d + m + 2 * n) - 0.10888 * Math.cos(2 * d + 2 * m - 2 * n);
  p += 2.3538 * Math.cos(2 * d + 2 * m - n) + 0.14764 * Math.cos(2 * d + 2 * m);
  p += 0.2556 * Math.cos(3 * d - m - n) - 0.15708 * Math.cos(3 * d - m);
  p += 0.86243 * Math.cos(3 * d - 2 * n) + 3.25823 * Math.cos(3 * d - n);
  p += 0.20099 * Math.cos(3 * d - 2 * f) - 1.41893 * Math.cos(3 * d);
  p -= 0.21259 * Math.cos(3 * d + m - n) + 0.10766 * Math.cos(3 * d + m);
  p -= 0.10834 * Math.cos(4 * d - 2 * m + 2 * n) - 0.27906 * Math.cos(4 * d - 2 * m - n);
  p -= 0.12806 * Math.cos(4 * d - 2 * m) + 1.897 * Math.cos(4 * d - m - 2 * n);
  p -= 3.95812 * Math.cos(4 * d - m - n) + 1.57145 * Math.cos(4 * d - m);
  p -= 0.20286 * Math.cos(4 * d - m + n) + 0.51423 * Math.cos(4 * d - 3 * n);
  p -= 21.63627 * Math.cos(4 * d - 2 * n) + 0.32176 * Math.cos(4 * d - n - 2 * f);
  p -= 34.78245 * Math.cos(4 * d - n) + 0.50793 * Math.cos(4 * d - 2 * f);
  p -= 11.64993 * Math.cos(4 * d) + 1.42255 * Math.cos(4 * d + n);
  p -= 0.13922 * Math.cos(4 * d + 2 * n) - 0.23696 * Math.cos(4 * d + m - 2 * n);
  p += 0.5788 * Math.cos(4 * d + m - n) + 0.24453 * Math.cos(4 * d + m);
  p -= 0.18316 * Math.cos(6 * d - 3 * n) + 0.4225 * Math.cos(6 * d - 2 * n);
  p -= 0.28663 * Math.cos(6 * d - n);

  distFromEarth = p;
  distEarthRadii[1] = distFromEarth / ER;

  document.getElementById("distmoon").innerHTML = fixedn(distFromEarth, 3) + "&nbsp;km";

/*  p = Math.sin(ER / distFromEarth) * R2D * 3600;
  p = Math.floor(p * 1000 + 0.5) / 1000;
  p1 = Math.floor(p / 60);
  document.getElementById("paramoon").innerHTML = p1 + "'&nbsp;" + zero(Math.floor((p - p1 * 60) * 1000 + 0.5) / 1000) + '"';*/

  diamapparent = Math.atan(MR / distFromEarth) * R2D * 60.0;
  document.getElementById("diamapparent").innerHTML = Math.floor(diamapparent) + "'&nbsp;" + zeroFixed2((diamapparent - Math.floor(diamapparent)) * 60) + '"';

  geocentricEquatorial(RADecMoon, obliquity, longitude_moon, latitude_moon);
  geocentric2topocentricEquatorial(RADecMoon, latObs, lonObs, elvObs, distFromEarth, jd, jde, true);
  showGeocentricEquatorialCoordinates(RADecMoon, "alphamoon", "deltamoon");

  z1 = Math.acos(Math.cos(latitude_moon) * Math.cos(longitude_moon - longitude_sun));
  if ( z1 < 0 )
    z1 += Math.PI;

   // Apparent magnitude (http://stjarnhimlen.se/comp/ppcomp.html#15)
  phase_angle = 180 - z1 * R2D - 0.1468 * Math.sin(z1) * ((1 - 0.0549 * Math.sin(anomaly_moon)) / (1 - 0.0167 * Math.sin(M)));
  phase = (1.0 + Math.cos(phase_angle * D2R)) * 50.0;
  var Rm = R - (distFromEarth * Math.cos(longitude_moon - longitude_sun) / AU);
  if ( phase <= 0.01 )
    magnitude = 26.5;
  else
    magnitude = 0.23 + (5 * Math.log(Rm * distFromEarth / AU) / Math.log(10.0)) + (0.026 * phase_angle) + (4.0E-9 * Math.pow(phase_angle, 4));
  document.getElementById("phasemoon").innerHTML = fixedn(phase, 1) + "%";
  document.getElementById("magmoon").innerHTML = fixedn(magnitude, 2);
  document.getElementById("rayonmoon").innerHTML = fixedn(Rm, 5) + ((language == "fr") ? "&nbsp;UA" : "&nbsp;AU");

  if (((longitude_moon < longitude_sun) && ((longitude_sun - longitude_moon) < Math.PI)) || ((longitude_moon > longitude_sun) && ((longitude_moon - longitude_sun) > Math.PI)))
  {
    if (language == "fr")
      elongation_moon = "Ouest";
    else
      elongation_moon = "West";
  }
  else
  {
    if (language == "fr")
      elongation_moon = "Est";
    else
      elongation_moon = "East";
  }

  document.getElementById("elongmoon").innerHTML = fixedn(z1 * R2D, 1) + "\xB0 ";
  document.getElementById("elongmoonEW").innerHTML = elongation_moon;

  if ( phase == 0 )
  {
    if (language == "fr")
    {
      phaseMoon = "Non visible";
      whenMoon = " C\u2019est la Nouvelle Lune";
    }
    else
    {
      phaseMoon = "Not visible";
      whenMoon = " New Moon";
    }
  }
  if ( (phase > 0) && (phase <= 7) )
  {
    if (language == "fr")
    {
      phaseMoon = "Non visible";
      whenMoon = " C\u2019est la Nouvelle Lune";
    }
    else
    {
      phaseMoon = "Not visible";
      whenMoon = " New Moon";
    }
  }
  if ( (phase > 7) && (phase <= 12) )
  {
    if (language == "fr")
    {
      phaseMoon = "Tr\xE8s fin croissant lunaire \xE0 tenter de voir";
      if ( elongation_moon == "Ouest" )
        whenMoon = " avant le lever du Soleil";
      else if ( elongation_moon == "Est" )
        whenMoon = " au coucher du Soleil";
    }
    else
    {
      phaseMoon = "Very thin Moon crescent";
      if ( elongation_moon == "West" )
        whenMoon = " before sunrise";
      else if ( elongation_moon == "East" )
        whenMoon = " at sunset";
    }
  }
  if ( (phase > 12) && (phase <= 15) )
  {
    if (language == "fr")
    {
      phaseMoon = "Fin croissant lunaire \xE0 tenter de voir";
      if ( elongation_moon == "Ouest" )
        whenMoon = " avant le lever du Soleil";
      else if ( elongation_moon == "Est" )
        whenMoon = " au coucher du Soleil";
    }
    else
    {
      phaseMoon = "Thin Moon crescent";
      if ( elongation_moon == "West" )
        whenMoon = " before sunrise";
      else if ( elongation_moon == "East" )
        whenMoon = " at sunset";
    }
  }
  if ( (phase > 15) && (phase <= 30) )
  {
    if (language == "fr")
    {
      phaseMoon = "Croissant de Lune \xE0 voir";
      if ( elongation_moon == "Ouest" )
        whenMoon = " \xE0 l\u2019aube";
      else if ( elongation_moon == "Est" )
        whenMoon = " au cr\xE9puscule";
    }
    else
    {
      phaseMoon = "Moon crescent";
      if ( elongation_moon == "West" )
        whenMoon = " at dawn";
      else if ( elongation_moon == "East" )
        whenMoon = " at twilight";
    }
  }
  if ( (phase > 30) && (phase <= 40) )
  {
    if (language == "fr")
    {
      phaseMoon = "Le gros croissant de Lune est";
      if ( elongation_moon == "Ouest" )
        whenMoon = " visible en fin de nuit";
      else if ( elongation_moon == "Est" )
        whenMoon = " visible en soir\xE9e";
    }
    else
    {
      phaseMoon = "Wide Moon crescent visible";
      if ( elongation_moon == "West" )
        whenMoon = " during the end of the night";
      else if ( elongation_moon == "East" )
        whenMoon = " during the evening";
    }
  }
  if ( (phase > 40) && (phase <= 60) )
  {
    if (language == "fr")
    {
      phaseMoon = "La Lune est en phase de";
      if ( elongation_moon == "Ouest" )
        whenMoon = " Dernier Quartier";
      else if ( elongation_moon == "Est" )
        whenMoon = " Premier Quartier";
    }
    else
    {
      phaseMoon = "Moon in the";
      if ( elongation_moon == "West" )
        whenMoon = " Last Quarter";
      else if ( elongation_moon == "East" )
        whenMoon = " First Quarter";
    }
  }
  if ( phase > 60 && phase <= 95)
  {
    if (language == "fr")
    {
      phaseMoon = "La Lune gibbeuse est";
      if ( elongation_moon == "Ouest" )
        whenMoon = " d\xE9croissante";
      else if ( elongation_moon == "Est" )
        whenMoon = " croissante";
    }
    else
    {
      phaseMoon = "The gibbous Moon is";
      if ( elongation_moon == "West" )
        whenMoon = " waning";
      else if ( elongation_moon == "East" )
        whenMoon = " waxing";
    }
  }
  if ( (phase > 95) && (phase <= 98) )
  {
    if (language == "fr")
    {
      phaseMoon = "La Lune est";
      if ( elongation_moon == "Ouest" )
        whenMoon = " encore bien Pleine";
      else if ( elongation_moon == "Est" )
        whenMoon = " presque Pleine";
    }
    else
    {
      phaseMoon = "The Moon is";
      if ( elongation_moon == "West" )
        whenMoon = " still quite Full";
      else if ( elongation_moon == "East" )
        whenMoon = " near Full";
    }
  }
  if ( (phase > 98) && (phase <= 100) )
  {
    if (language == "fr")
      phaseMoon = "Pleine Lune";
    else
      phaseMoon = "Full Moon";
    whenMoon = "";
  }

  da_Moon_Merc = Math.sin(RADecMoon.geoDec) * Math.sin(RADecMerc.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecMerc.geoDec) * Math.cos(RADecMoon.geoRA - RADecMerc.geoRA);
  da_Moon_Merc = Math.acos(da_Moon_Merc) * R2D;
  if ( da_Moon_Merc < 10 )
  {
    if (language == "fr")
      da_Moon_Merc = ", \xE0 " + Math.floor(da_Moon_Merc) + "\xB0&nbsp;" + Math.round(((da_Moon_Merc - Math.floor(da_Moon_Merc)) * 60)) + "' de Mercure";
    else
      da_Moon_Merc = ", " + Math.floor(da_Moon_Merc) + "\xB0&nbsp;" + Math.round(((da_Moon_Merc - Math.floor(da_Moon_Merc)) * 60)) + "' from Mercury";
  }
  else
    da_Moon_Merc = "";

  da_Moon_Ven = Math.sin(RADecMoon.geoDec) * Math.sin(RADecVen.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecVen.geoDec) * Math.cos(RADecMoon.geoRA - RADecVen.geoRA)
  da_Moon_Ven = Math.acos(da_Moon_Ven) * R2D;
  if ( da_Moon_Ven < 10 )
  {
    if (language == "fr")
      da_Moon_Ven = ", \xE0 " + Math.floor(da_Moon_Ven) + "\xB0&nbsp;" + Math.round(((da_Moon_Ven - Math.floor(da_Moon_Ven)) * 60)) + "' de V\xE9nus";
    else
      da_Moon_Ven = ", " + Math.floor(da_Moon_Ven) + "\xB0&nbsp;" + Math.round(((da_Moon_Ven - Math.floor(da_Moon_Ven)) * 60)) + "' from Venus";
  }
  else
    da_Moon_Ven = "";

  da_Moon_Mars = Math.sin(RADecMoon.geoDec) * Math.sin(RADecMars.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecMars.geoDec) * Math.cos(RADecMoon.geoRA - RADecMars.geoRA)
  da_Moon_Mars = Math.acos(da_Moon_Mars) * R2D;
  if ( da_Moon_Mars < 10 )
  {
    if (language == "fr")
      da_Moon_Mars = ", \xE0 " + Math.floor(da_Moon_Mars) + "\xB0&nbsp;" + Math.round(((da_Moon_Mars - Math.floor(da_Moon_Mars)) * 60)) + "' de Mars";
    else
      da_Moon_Mars = ", " + Math.floor(da_Moon_Mars) + "\xB0&nbsp;" + Math.round(((da_Moon_Mars - Math.floor(da_Moon_Mars)) * 60)) + "' from Mars";
  }
  else
    da_Moon_Mars = "";

  da_Moon_Jup = Math.sin(RADecMoon.geoDec) * Math.sin(RADecJup.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecJup.geoDec) * Math.cos(RADecMoon.geoRA - RADecJup.geoRA)
  da_Moon_Jup = Math.acos(da_Moon_Jup) * R2D;
  if ( da_Moon_Jup < 10 )
  {
    if (language == "fr")
      da_Moon_Jup = ", \xE0 " + Math.floor(da_Moon_Jup) + "\xB0&nbsp;" + Math.round(((da_Moon_Jup - Math.floor(da_Moon_Jup)) * 60)) + "' de Jupiter";
    else
      da_Moon_Jup = ", " + Math.floor(da_Moon_Jup) + "\xB0&nbsp;" + Math.round(((da_Moon_Jup - Math.floor(da_Moon_Jup)) * 60)) + "' from Jupiter";
  }
  else
    da_Moon_Jup = "";

  da_Moon_Sat = Math.sin(RADecMoon.geoDec) * Math.sin(RADecSat.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecSat.geoDec) * Math.cos(RADecMoon.geoRA - RADecSat.geoRA)
  da_Moon_Sat = Math.acos(da_Moon_Sat) * R2D;
  if ( da_Moon_Sat < 10 )
  {
    if (language == "fr")
      da_Moon_Sat = ", \xE0 " + Math.floor(da_Moon_Sat) + "\xB0&nbsp;" + Math.round(((da_Moon_Sat - Math.floor(da_Moon_Sat)) * 60)) + "' de Saturne";
    else
      da_Moon_Sat = ", " + Math.floor(da_Moon_Sat) + "\xB0&nbsp;" + Math.round(((da_Moon_Sat - Math.floor(da_Moon_Sat)) * 60)) + "' from Saturne";
  }
  else
    da_Moon_Sat = "";

  da_Moon_Uran = Math.sin(RADecMoon.geoDec) * Math.sin(RADecUran.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecUran.geoDec) * Math.cos(RADecMoon.geoRA - RADecUran.geoRA)
  da_Moon_Uran = Math.acos(da_Moon_Uran) * R2D;
  if ( da_Moon_Uran < 10 )
  {
    if (language == "fr")
      da_Moon_Uran = ", \xE0 " + Math.floor(da_Moon_Uran) + "\xB0&nbsp;" + Math.round(((da_Moon_Uran - Math.floor(da_Moon_Uran)) * 60)) + "' de Uranus";
    else
      da_Moon_Uran = ", " + Math.floor(da_Moon_Uran) + "\xB0&nbsp;" + Math.round(((da_Moon_Uran - Math.floor(da_Moon_Uran)) * 60)) + "' from Uranus";
  }
  else
    da_Moon_Uran = "";

  da_Moon_Nept = Math.sin(RADecMoon.geoDec) * Math.sin(RADecNept.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecNept.geoDec) * Math.cos(RADecMoon.geoRA - RADecNept.geoRA)
  da_Moon_Nept = Math.acos(da_Moon_Nept) * R2D;
  if ( da_Moon_Nept < 10 )
  {
    if (language == "fr")
      da_Moon_Nept = ", \xE0 " + Math.floor(da_Moon_Nept) + "\xB0&nbsp;" + Math.round(((da_Moon_Nept - Math.floor(da_Moon_Nept)) * 60)) + "' de Neptune";
    else
      da_Moon_Nept = ", " + Math.floor(da_Moon_Nept) + "\xB0&nbsp;" + Math.round(((da_Moon_Nept - Math.floor(da_Moon_Nept)) * 60)) + "' from Neptune";
  }
  else
    da_Moon_Nept = "";

  da_Moon_Pluto = Math.sin(RADecMoon.geoDec) * Math.sin(RADecPluto.geoDec) + Math.cos(RADecMoon.geoDec) * Math.cos(RADecPluto.geoDec) * Math.cos(RADecMoon.geoRA - RADecPluto.geoRA)
  da_Moon_Pluto = Math.acos(da_Moon_Pluto) * R2D;
  if ( da_Moon_Pluto < 10 )
  {
    if (language == "fr")
      da_Moon_Pluto = ", \xE0 " + Math.floor(da_Moon_Pluto) + "\xB0&nbsp;" + Math.round(((da_Moon_Pluto - Math.floor(da_Moon_Pluto)) * 60)) + "' de Pluto";
    else
      da_Moon_Pluto = ", " + Math.floor(da_Moon_Pluto) + "\xB0&nbsp;" + Math.round(((da_Moon_Pluto - Math.floor(da_Moon_Pluto)) * 60)) + "' from Pluto";
  }
  else
    da_Moon_Pluto = "";

  document.getElementById("visibmoon").innerHTML = phaseMoon + whenMoon + da_Moon_Merc + da_Moon_Ven + da_Moon_Mars + da_Moon_Jup + da_Moon_Sat + da_Moon_Uran + da_Moon_Nept + da_Moon_Pluto;

  var RA = new Array(10);
  var Dec = new Array(10);
  RA[0] = RADecSun.geoRA;
  Dec[0] = RADecSun.geoDec;
  RA[1] = RADecMoon.geoRA;
  Dec[1] = RADecMoon.geoDec;
  RA[2] = RADecMerc.geoRA;
  Dec[2] = RADecMerc.geoDec;
  RA[3] = RADecVen.geoRA;
  Dec[3] = RADecVen.geoDec;
  RA[4] = RADecMars.geoRA;
  Dec[4] = RADecMars.geoDec;
  RA[5] = RADecJup.geoRA;
  Dec[5] = RADecJup.geoDec;
  RA[6] = RADecSat.geoRA;
  Dec[6] = RADecSat.geoDec;
  RA[7] = RADecUran.geoRA;
  Dec[7] = RADecUran.geoDec;
  RA[8] = RADecNept.geoRA;
  Dec[8] = RADecNept.geoDec;
  RA[9] = RADecPluto.geoRA;
  Dec[9] = RADecPluto.geoDec;

   // Angular separation
  var angsepsu = new Array(10);
  var angsepmo = new Array(10);
  var angsepme = new Array(10);
  var angsepve = new Array(10);
  var angsepma = new Array(10);
  var angsepju = new Array(10);
  var angsepsa = new Array(10);
  var angsepur = new Array(10);
  var angsepne = new Array(10);
  var angseppl = new Array(10);

  for ( indx = 0; indx < 10; indx++ )
  {
    ax = 0;
    angsepsu[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepmo[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepme[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepve[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepma[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepju[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepsa[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepur[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angsepne[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
    ax += 1;
    angseppl[indx] = aff(R2D * Math.acos(Math.sin(Dec[indx]) * Math.sin(Dec[ax]) + Math.cos(Dec[indx]) * Math.cos(Dec[ax]) * Math.cos(RA[indx] - RA[ax])));
  }

  document.getElementById("sumo").innerHTML = angsepsu[1];
  document.getElementById("sume").innerHTML = angsepsu[2];
  document.getElementById("suve").innerHTML = angsepsu[3];
  document.getElementById("suma").innerHTML = angsepsu[4];
  document.getElementById("suju").innerHTML = angsepsu[5];
  document.getElementById("susa").innerHTML = angsepsu[6];
  document.getElementById("suur").innerHTML = angsepsu[7];
  document.getElementById("sune").innerHTML = angsepsu[8];
  document.getElementById("supl").innerHTML = angsepsu[9];

  document.getElementById("mome").innerHTML = angsepmo[0];
  document.getElementById("mosu").innerHTML = angsepmo[2];
  document.getElementById("move").innerHTML = angsepmo[3];
  document.getElementById("moma").innerHTML = angsepmo[4];
  document.getElementById("moju").innerHTML = angsepmo[5];
  document.getElementById("mosa").innerHTML = angsepmo[6];
  document.getElementById("mour").innerHTML = angsepmo[7];
  document.getElementById("mone").innerHTML = angsepmo[8];
  document.getElementById("mopl").innerHTML = angsepmo[9];

  document.getElementById("mesu").innerHTML = angsepme[0];
  document.getElementById("memo").innerHTML = angsepme[1];
  document.getElementById("meve").innerHTML = angsepme[3];
  document.getElementById("mema").innerHTML = angsepme[4];
  document.getElementById("meju").innerHTML = angsepme[5];
  document.getElementById("mesa").innerHTML = angsepme[6];
  document.getElementById("meur").innerHTML = angsepme[7];
  document.getElementById("mene").innerHTML = angsepme[8];
  document.getElementById("mepl").innerHTML = angsepme[9];

  document.getElementById("vesu").innerHTML = angsepve[0];
  document.getElementById("vemo").innerHTML = angsepve[1];
  document.getElementById("veme").innerHTML = angsepve[2];
  document.getElementById("vema").innerHTML = angsepve[4];
  document.getElementById("veju").innerHTML = angsepve[5];
  document.getElementById("vesa").innerHTML = angsepve[6];
  document.getElementById("veur").innerHTML = angsepve[7];
  document.getElementById("vene").innerHTML = angsepve[8];
  document.getElementById("vepl").innerHTML = angsepve[9];

  document.getElementById("masu").innerHTML = angsepma[0];
  document.getElementById("mamo").innerHTML = angsepma[1];
  document.getElementById("mame").innerHTML = angsepma[2];
  document.getElementById("mave").innerHTML = angsepma[3];
  document.getElementById("maju").innerHTML = angsepma[5];
  document.getElementById("masa").innerHTML = angsepma[6];
  document.getElementById("maur").innerHTML = angsepma[7];
  document.getElementById("mane").innerHTML = angsepma[8];
  document.getElementById("mapl").innerHTML = angsepma[9];

  document.getElementById("jusu").innerHTML = angsepju[0];
  document.getElementById("jumo").innerHTML = angsepju[1];
  document.getElementById("jume").innerHTML = angsepju[2];
  document.getElementById("juve").innerHTML = angsepju[3];
  document.getElementById("juma").innerHTML = angsepju[4];
  document.getElementById("jusa").innerHTML = angsepju[6];
  document.getElementById("juur").innerHTML = angsepju[7];
  document.getElementById("june").innerHTML = angsepju[8];
  document.getElementById("jupl").innerHTML = angsepju[9];

  document.getElementById("sasu").innerHTML = angsepsa[0];
  document.getElementById("samo").innerHTML = angsepsa[1];
  document.getElementById("same").innerHTML = angsepsa[2];
  document.getElementById("save").innerHTML = angsepsa[3];
  document.getElementById("sama").innerHTML = angsepsa[4];
  document.getElementById("saju").innerHTML = angsepsa[5];
  document.getElementById("saur").innerHTML = angsepsa[7];
  document.getElementById("sane").innerHTML = angsepsa[8];
  document.getElementById("sapl").innerHTML = angsepsa[9];

  document.getElementById("ursu").innerHTML = angsepur[0];
  document.getElementById("urmo").innerHTML = angsepur[1];
  document.getElementById("urme").innerHTML = angsepur[2];
  document.getElementById("urve").innerHTML = angsepur[3];
  document.getElementById("urma").innerHTML = angsepur[4];
  document.getElementById("urju").innerHTML = angsepur[5];
  document.getElementById("ursa").innerHTML = angsepur[6];
  document.getElementById("urne").innerHTML = angsepur[8];
  document.getElementById("urpl").innerHTML = angsepur[9];

  document.getElementById("nesu").innerHTML = angsepne[0];
  document.getElementById("nemo").innerHTML = angsepne[1];
  document.getElementById("neme").innerHTML = angsepne[2];
  document.getElementById("neve").innerHTML = angsepne[3];
  document.getElementById("nema").innerHTML = angsepne[4];
  document.getElementById("neju").innerHTML = angsepne[5];
  document.getElementById("nesa").innerHTML = angsepne[6];
  document.getElementById("neur").innerHTML = angsepne[7];
  document.getElementById("nepl").innerHTML = angsepne[9];

  document.getElementById("plsu").innerHTML = angseppl[0];
  document.getElementById("plmo").innerHTML = angseppl[1];
  document.getElementById("plme").innerHTML = angseppl[2];
  document.getElementById("plve").innerHTML = angseppl[3];
  document.getElementById("plma").innerHTML = angseppl[4];
  document.getElementById("plju").innerHTML = angseppl[5];
  document.getElementById("plsa").innerHTML = angseppl[6];
  document.getElementById("plur").innerHTML = angseppl[7];
  document.getElementById("plne").innerHTML = angseppl[8];

   //-----
   // Sun rise, set and transit
   //-----

  var efr0 = -0.009890199;        // Refraction on the horizon - given value of 34'
  var efr1 = -0.004654211;        // Refraction on the horizon - Sun's semi-diameter of 16'
  var ht = efr0 + efr1;           // Refraction on the horizon for the Sun
  var parallaxMoon = 0.016580628; // Moon's parallax
  var sdMoon = 0.004654211;       // Moon's semi-diameter
  var ht0 = parallaxMoon - efr0 - sdMoon;

   // UT noon time
  h = 12 + (lonObs / H2R);

   // Equatorial coordinates
  var xe = Math.cos(true_long);
  var ye = Math.cos(obliquity) * Math.sin(true_long);
  var ze = Math.sin(obliquity) * Math.sin(true_long);

  var rx = (Math.cos(L) * xe) + (Math.sin(L) * ye);
  var ry = -(Math.sin(L) * xe) + (Math.cos(L) * ye);

  var et = Math.atan(ry / rx);
  var dc = Math.atan(ze / Math.sqrt(1 - (ze * ze)));
//  var decl = Math.atan(ze / Math.sqrt(1 - (ze * ze))) * R2D;

   // Time of transit
  var pm = h + (et / H2R);
  var merh = Math.floor(pm);
  pm = 60 * (pm - merh);
  var merm = Math.floor(pm);
  pm = Math.floor(60 * (pm - merm));
  if (language == "fr")
    document.getElementById("mmeridian").innerHTML = notn(zero(merh)) + "h" + notn(zero(merm)) + "m";
  else
    document.getElementById("mmeridian").innerHTML = notn(zero(((merh > 12) ? merh - 12 : merh))) + ":" + notn(zero(merm)) + ((merh > 12) ? "pm" : "am");

   // Hour angle at sunrise and sunset, etc
  var riseh = new Array();
  var risem = new Array();
  var seth = new Array();
  var setm = new Array();
  var htSun = new Array();
  htSun[0] = ht;
  htSun[1] = -6 * D2R;        // Civil twilight (6 degrees below the horizon)
  htSun[2] = -12 * D2R;       // Nautical twilight (12 degrees below the horizon)
  htSun[3] = -18 * D2R;       // Astronomical twilight (18 degrees below the horizon)

  for ( var i = 0; i < 4; i++ )
  {
    var cs = (Math.sin(htSun[i]) - Math.sin(latObs) * Math.sin(dc)) / Math.cos(latObs) / Math.cos(dc);
    if ( cs == 0 )
      ha = Math.PI / 2;
    else
      ha = Math.atan(Math.sqrt(1 - cs * cs) / cs);
    if ( cs < 0 )
      ha += Math.PI;
    var pm1 = h + ((et - ha) / H2R);
    if ( pm1 < 0 )
      pm1 += 24;
    riseh[i] = Math.floor(pm1);
    risem[i] = Math.floor(60 * (pm1 - riseh[i]));
    var pm2 = h + ((et + ha) / H2R);
    if ( pm2 > 24 )
      pm2 -= 24;
    seth[i] = Math.floor(pm2);
    setm[i] = Math.floor(60 * (pm2 - seth[i]));
  }

  if (language == "fr")
  {
    document.getElementById("mrise").innerHTML = notn(zero(riseh[0])) + "h" + notn(zero(risem[0])) + "m";
    document.getElementById("mset").innerHTML = notn(zero(seth[0])) + "h" + notn(zero(setm[0])) + "m";
    document.getElementById("ccm").innerHTML = notn(zero(riseh[1])) + "h" + notn(zero(risem[1])) + "m";
    document.getElementById("ccs").innerHTML = notn(zero(seth[1])) + "h" + notn(zero(setm[1])) + "m";
    document.getElementById("cnm").innerHTML = notn(zero(riseh[2])) + "h" + notn(zero(risem[2])) + "m";
    document.getElementById("cns").innerHTML = notn(zero(seth[2])) + "h" + notn(zero(setm[2])) + "m";
    document.getElementById("cam").innerHTML = notn(zero(riseh[3])) + "h" + notn(zero(risem[3])) + "m";
    document.getElementById("cas").innerHTML = notn(zero(seth[3])) + "h" + notn(zero(setm[3])) + "m";
  }
  else
  {
    document.getElementById("mrise").innerHTML = notn(zero(((riseh[0] > 12) ? riseh[0] - 12 : riseh[0]))) + ":" + notn(zero(risem[0])) + ((riseh[0] > 12) ? "pm" : "am");
    document.getElementById("mset").innerHTML = notn(zero(((seth[0] > 12) ? seth[0] - 12 : seth[0]))) + ":" + notn(zero(setm[0])) + ((seth[0] > 12) ? "pm" : "am");
    document.getElementById("ccm").innerHTML = notn(zero(((riseh[1] > 12) ? riseh[1] - 12 : riseh[1]))) + ":" + notn(zero(risem[1])) + ((riseh[1] > 12) ? "pm" : "am");
    document.getElementById("ccs").innerHTML = notn(zero(((seth[1] > 12) ? seth[1] - 12 : seth[1]))) + ":" + notn(zero(setm[1])) + ((seth[1] > 12) ? "pm" : "am");
    document.getElementById("cnm").innerHTML = notn(zero(((riseh[2] > 12) ? riseh[2] - 12 : riseh[2]))) + ":" + notn(zero(risem[2])) + ((riseh[2] > 12) ? "pm" : "am");
    document.getElementById("cns").innerHTML = notn(zero(((seth[2] > 12) ? seth[2] - 12 : seth[2]))) + ":" + notn(zero(setm[2])) + ((seth[2] > 12) ? "pm" : "am");
    document.getElementById("cam").innerHTML = notn(zero(((riseh[3] > 12) ? riseh[3] - 12 : riseh[3]))) + ":" + notn(zero(risem[3])) + ((riseh[3] > 12) ? "pm" : "am");
    document.getElementById("cas").innerHTML = notn(zero(((seth[3] > 12) ? seth[3] - 12 : seth[3]))) + ":" + notn(zero(setm[3])) + ((seth[3] > 12) ? "pm" : "am");
  }

   // Topocentric altitude and azimut
  var alt = new Array();
  var az = new Array();
//  var num, denom;

  var HA1 = (GMST * 15.0 * D2R) + (-lonObs);
  for ( var ind = 0; ind < 10; ind++ )
  {
    var HA = HA1 - RA[ind];	// Local hour angle
    alt[ind] = Math.asin(Math.sin(Dec[ind]) * Math.sin(latObs) + Math.cos(Dec[ind]) * Math.cos(HA) * Math.cos(latObs)) * R2D; // Geocentric
    alt[ind] = geocentric2topocentricAltAz(alt[ind], distEarthRadii[ind]);
    if ( alt[ind] > 0 )
    {
      az[ind] = Math.atan2(Math.sin(HA) * Math.cos(Dec[ind]), (Math.cos(HA) * Math.sin(latObs) * Math.cos(Dec[ind])) - (Math.sin(Dec[ind]) * Math.cos(latObs))) * R2D;
      az[ind] += 180;

       // Keep the azimut between 0 and 360 degrees
      if ( az[ind] < 0 )
        az[ind] += 360;
      else if ( az[ind] > 360 )
        az[ind] -= 360;
/*      if ( latx == 1 ) // North
      {
        num = Math.sin(HA);
        denom = (Math.cos(HA) * Math.sin(latObs)) - (Math.tan(Dec[ind]) * Math.cos(latObs));
        azimut = num / denom;
        az[ind] = 180 + (Math.atan(azimut) * R2D);
        if ( denom < 0 )
          az[ind] += 180;
      }
      else // South
      {
        num = Math.sin(-HA);
        denom = (Math.cos(-HA) * Math.sin(-latObs)) - (Math.tan(-Dec[ind]) * Math.cos(-latObs));
        azimut = num / denom;
        az[ind] = 180 - (Math.atan(-azimut) * R2D);
        if ( denom > 0 )
          az[ind] -= 180;
      }

       // Keep the azimut between 0 and 360 degrees
      if ( az[ind] < 0 )
        az[ind] += 360;
      if ( az[ind] > 360 )
        az[ind] -= 360;*/

/*      z1 = ((az[ind] - Math.floor(az[ind])) * 60);
      z2 = ((z1 - Math.floor(z1)) * 60);
      az[ind] = Math.floor(az[ind]) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';*/
      az[ind] = zeroFixed2(az[ind]) + "\xB0&nbsp;";
      sign = "&nbsp;";
    }
    else
    {
      az[ind] = "---";
      sign = "-";
    }

    d = Math.abs(alt[ind]);
/*    z1 = ((d - Math.floor(d)) * 60);
    z2 = ((z1 - Math.floor(z1)) * 60);
    alt[ind] = sign + Math.floor(d) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';*/
    alt[ind] = sign + zeroFixed2(d) + "\xB0&nbsp;";
  }

  document.getElementById("altSun").innerHTML = alt[0];
  document.getElementById("azSun").innerHTML = az[0];
  document.getElementById("altMoon").innerHTML = alt[1];
  document.getElementById("azMoon").innerHTML = az[1];
  document.getElementById("altMerc").innerHTML = alt[2];
  document.getElementById("azMerc").innerHTML = az[2];
  document.getElementById("altVen").innerHTML = alt[3];
  document.getElementById("azVen").innerHTML = az[3];
  document.getElementById("altMars").innerHTML = alt[4];
  document.getElementById("azMars").innerHTML = az[4];
  document.getElementById("altJup").innerHTML = alt[5];
  document.getElementById("azJup").innerHTML = az[5];
  document.getElementById("altSat").innerHTML = alt[6];
  document.getElementById("azSat").innerHTML = az[6];
  document.getElementById("altUran").innerHTML = alt[7];
  document.getElementById("azUran").innerHTML = az[7];
  document.getElementById("altNept").innerHTML = alt[8];
  document.getElementById("azNept").innerHTML = az[8];
  document.getElementById("altPluto").innerHTML = alt[9];
  document.getElementById("azPluto").innerHTML = az[9];

   // Rise and set of the planets
  var risehP = new Array(10);
  var risemP = new Array(10);
  var sethP = new Array(10);
  var setmP = new Array(10);
  var transithP = new Array(10);
  var transitmP = new Array(10);

  for ( var index = 0; index < 10; index++ )
  {
    if ( index == 0 )
      var csP = (Math.sin(ht0) - Math.sin(latObs) * Math.sin(Dec[index])) / Math.cos(latObs) / Math.cos(Dec[index]);
    else
      var csP = (Math.sin(efr0) - Math.sin(latObs) * Math.sin(Dec[index])) / Math.cos(latObs) / Math.cos(Dec[index]);
    AH = Math.acos(csP);

     // Rise
    TSl = (RA[index] - AH) * R2H;
    T1l = TSl - (-lonObs * R2H);
    Ttl = T1l - GMST0;
    if ( Ttl < 0 )
      Ttl += 24;
    Ttl /= INVERSE_SIDEREAL_DAY;
    if ( Ttl < 0 )
      Ttl += 24;
    else if ( Ttl > 24 )
      Ttl -= 24;
    risehP[index] = Math.floor(Ttl);
    risemP[index] = Math.floor(60 * (Ttl - risehP[index]));

     // Set
    TSc = (RA[index] + AH) * R2H;
    T1c = TSc - (-lonObs * R2H);
    Ttc = T1c - GMST0;
    if ( Ttc < 0 )
      Ttc += 24;
    Ttc /= INVERSE_SIDEREAL_DAY;
    if ( Ttc < 0 )
      Ttc += 24;
    else if ( Ttc > 24 )
      Ttc -= 24;
    sethP[index] = Math.floor(Ttc);
    setmP[index] = Math.floor(60 * (Ttc - sethP[index]));

     // Transit
    if ( Ttc < Ttl )
      Ttc += 24;
    Tt = (Ttl + Ttc) / 2;
    if ( Tt > 24 )
      Tt -= 24;
    transithP[index] = Math.floor(Tt);
    transitmP[index] = Math.floor(60 * (Tt - transithP[index]));
  }

  if (language == "fr")
  {
    document.getElementById("risMe").innerHTML = notn(zero(risehP[2])) + "h" + notn(zero(risemP[2])) + "m";
    document.getElementById("traMe").innerHTML = notn(zero(transithP[2])) + "h" + notn(zero(transitmP[2])) + "m";
    document.getElementById("setMe").innerHTML = notn(zero(sethP[2])) + "h" + notn(zero(setmP[2])) + "m";

    document.getElementById("risVe").innerHTML = notn(zero(risehP[3])) + "h" + notn(zero(risemP[3])) + "m";
    document.getElementById("traVe").innerHTML = notn(zero(transithP[3])) + "h" + notn(zero(transitmP[3])) + "m";
    document.getElementById("setVe").innerHTML = notn(zero(sethP[3])) + "h" + notn(zero(setmP[3])) + "m";

    document.getElementById("risMa").innerHTML = notn(zero(risehP[4])) + "h" + notn(zero(risemP[4])) + "m";
    document.getElementById("traMa").innerHTML = notn(zero(transithP[4])) + "h" + notn(zero(transitmP[4])) + "m";
    document.getElementById("setMa").innerHTML = notn(zero(sethP[4])) + "h" + notn(zero(setmP[4])) + "m";

    document.getElementById("risJu").innerHTML = notn(zero(risehP[5])) + "h" + notn(zero(risemP[5])) + "m";
    document.getElementById("traJu").innerHTML = notn(zero(transithP[5])) + "h" + notn(zero(transitmP[5])) + "m";
    document.getElementById("setJu").innerHTML = notn(zero(sethP[5])) + "h" + notn(zero(setmP[5])) + "m";

    document.getElementById("risSa").innerHTML = notn(zero(risehP[6])) + "h" + notn(zero(risemP[6])) + "m";
    document.getElementById("traSa").innerHTML = notn(zero(transithP[6])) + "h" + notn(zero(transitmP[6])) + "m";
    document.getElementById("setSa").innerHTML = notn(zero(sethP[6])) + "h" + notn(zero(setmP[6])) + "m";

    document.getElementById("risUr").innerHTML = notn(zero(risehP[7])) + "h" + notn(zero(risemP[7])) + "m";
    document.getElementById("traUr").innerHTML = notn(zero(transithP[7])) + "h" + notn(zero(transitmP[7])) + "m";
    document.getElementById("setUr").innerHTML = notn(zero(sethP[7])) + "h" + notn(zero(setmP[7])) + "m";

    document.getElementById("risNe").innerHTML = notn(zero(risehP[8])) + "h" + notn(zero(risemP[8])) + "m";
    document.getElementById("traNe").innerHTML = notn(zero(transithP[8])) + "h" + notn(zero(transitmP[8])) + "m";
    document.getElementById("setNe").innerHTML = notn(zero(sethP[8])) + "h" + notn(zero(setmP[8])) + "m";

    document.getElementById("risPl").innerHTML = notn(zero(risehP[9])) + "h" + notn(zero(risemP[9])) + "m";
    document.getElementById("traPl").innerHTML = notn(zero(transithP[9])) + "h" + notn(zero(transitmP[9])) + "m";
    document.getElementById("setPl").innerHTML = notn(zero(sethP[9])) + "h" + notn(zero(setmP[9])) + "m";
  }
  else
  {
    document.getElementById("risMe").innerHTML = notn(zero(((risehP[2] > 12) ? risehP[2] - 12 : risehP[2]))) + ":" + notn(zero(risemP[2])) + ((risehP[2] > 12) ? "pm" : "am");
    document.getElementById("traMe").innerHTML = notn(zero(((transithP[2] > 12) ? transithP[2] - 12 : transithP[2]))) + ":" + notn(zero(transitmP[2])) + ((transithP[2] > 12) ? "pm" : "am");
    document.getElementById("setMe").innerHTML = notn(zero(((sethP[2] > 12) ? sethP[2] - 12 : sethP[2]))) + ":" + notn(zero(setmP[2])) + ((sethP[2] > 12) ? "pm" : "am");

    document.getElementById("risVe").innerHTML = notn(zero(((risehP[3] > 12) ? risehP[3] - 12 : risehP[3]))) + ":" + notn(zero(risemP[3])) + ((risehP[3] > 12) ? "pm" : "am");
    document.getElementById("traVe").innerHTML = notn(zero(((transithP[3] > 12) ? transithP[3] - 12 : transithP[3]))) + ":" + notn(zero(transitmP[3])) + ((transithP[3] > 12) ? "pm" : "am");
    document.getElementById("setVe").innerHTML = notn(zero(((sethP[3] > 12) ? sethP[3] - 12 : sethP[3]))) + ":" + notn(zero(setmP[3])) + ((sethP[3] > 12) ? "pm" : "am");

    document.getElementById("risMa").innerHTML = notn(zero(((risehP[4] > 12) ? risehP[4] - 12 : risehP[4]))) + ":" + notn(zero(risemP[4])) + ((risehP[4] > 12) ? "pm" : "am");
    document.getElementById("traMa").innerHTML = notn(zero(((transithP[4] > 12) ? transithP[4] - 12 : transithP[4]))) + ":" + notn(zero(transitmP[4])) + ((transithP[4] > 12) ? "pm" : "am");
    document.getElementById("setMa").innerHTML = notn(zero(((sethP[4] > 12) ? sethP[4] - 12 : sethP[4]))) + ":" + notn(zero(setmP[4])) + ((sethP[4] > 12) ? "pm" : "am");

    document.getElementById("risJu").innerHTML = notn(zero(((risehP[5] > 12) ? risehP[5] - 12 : risehP[5]))) + ":" + notn(zero(risemP[5])) + ((risehP[5] > 12) ? "pm" : "am");
    document.getElementById("traJu").innerHTML = notn(zero(((transithP[5] > 12) ? transithP[5] - 12 : transithP[5]))) + ":" + notn(zero(transitmP[5])) + ((transithP[5] > 12) ? "pm" : "am");
    document.getElementById("setJu").innerHTML = notn(zero(((sethP[5] > 12) ? sethP[5] - 12 : sethP[5]))) + ":" + notn(zero(setmP[5])) + ((sethP[5] > 12) ? "pm" : "am");

    document.getElementById("risSa").innerHTML = notn(zero(((risehP[6] > 12) ? risehP[6] - 12 : risehP[6]))) + ":" + notn(zero(risemP[6])) + ((risehP[6] > 12) ? "pm" : "am");
    document.getElementById("traSa").innerHTML = notn(zero(((transithP[6] > 12) ? transithP[6] - 12 : transithP[6]))) + ":" + notn(zero(transitmP[6])) + ((transithP[6] > 12) ? "pm" : "am");
    document.getElementById("setSa").innerHTML = notn(zero(((sethP[6] > 12) ? sethP[6] - 12 : sethP[6]))) + ":" + notn(zero(setmP[6])) + ((sethP[6] > 12) ? "pm" : "am");

    document.getElementById("risUr").innerHTML = notn(zero(((risehP[7] > 12) ? risehP[7] - 12 : risehP[7]))) + ":" + notn(zero(risemP[7])) + ((risehP[7] > 12) ? "pm" : "am");
    document.getElementById("traUr").innerHTML = notn(zero(((transithP[7] > 12) ? transithP[7] - 12 : transithP[7]))) + ":" + notn(zero(transitmP[7])) + ((transithP[7] > 12) ? "pm" : "am");
    document.getElementById("setUr").innerHTML = notn(zero(((sethP[7] > 12) ? sethP[7] - 12 : sethP[7]))) + ":" + notn(zero(setmP[7])) + ((sethP[7] > 12) ? "pm" : "am");

    document.getElementById("risNe").innerHTML = notn(zero(((risehP[8] > 12) ? risehP[8] - 12 : risehP[8]))) + ":" + notn(zero(risemP[8])) + ((risehP[8] > 12) ? "pm" : "am");
    document.getElementById("traNe").innerHTML = notn(zero(((transithP[8] > 12) ? transithP[8] - 12 : transithP[8]))) + ":" + notn(zero(transitmP[8])) + ((transithP[8] > 12) ? "pm" : "am");
    document.getElementById("setNe").innerHTML = notn(zero(((sethP[8] > 12) ? sethP[8] - 12 : sethP[8]))) + ":" + notn(zero(setmP[8])) + ((sethP[8] > 12) ? "pm" : "am");

    document.getElementById("risPl").innerHTML = notn(zero(((risehP[9] > 12) ? risehP[9] - 12 : risehP[9]))) + ":" + notn(zero(risemP[9])) + ((risehP[9] > 12) ? "pm" : "am");
    document.getElementById("traPl").innerHTML = notn(zero(((transithP[9] > 12) ? transithP[9] - 12 : transithP[9]))) + ":" + notn(zero(transitmP[9])) + ((transithP[9] > 12) ? "pm" : "am");
    document.getElementById("setPl").innerHTML = notn(zero(((sethP[9] > 12) ? sethP[9] - 12 : sethP[9]))) + ":" + notn(zero(setmP[9])) + ((sethP[9] > 12) ? "pm" : "am");
  }

  moon_data(language);

  if (language == "fr")
    window.status = "Termin&eacute;";
  else
    window.status = "Finished";
}

// Convert epoch JNow to J2000 (Google Sky)
// Valid for the Sun and planet only
function JNow2J2000( jd0, obj, delta_t )
{
  var T_Start = (jd0 - 2451545.0) / 36525.0;
  var T2_Start = T_Start * T_Start;
  var jd1 = julianDay(1, 1, 2000, 12, 0, 0) + (delta_t / 86400);
  var T_End = (jd1 - jd0) / 36525.0;
  var T2_End = T_End * T_End;
  var T3_End = T2_End * T_End;

  var zeta = ((2306.2181 + (1.39656 * T_Start) - (0.000139 * T2_Start)) * T_End) + ((0.30188 - (0.000344 * T_Start)) * T2_End) + (0.017998 * T3_End);
  zeta *= D2R / 3600.0;
  var z = ((2306.2181 + (1.39656 * T_Start) - (0.000139 * T2_Start)) * T_End) + ((1.09468 + (0.000066 * T_Start)) * T2_End) + (0.018203 * T3_End);
  z *= D2R / 3600.0;
  var theta = ((2004.3109 - (0.85330 * T_Start) - (0.000217 * T2_Start)) * T_End) - ((0.42665 + (0.000217 * T_Start)) * T2_End) - (0.041833 * T3_End);
  theta *= D2R / 3600.0;

  var A = Math.cos(obj.topoDec) * Math.sin(obj.topoRA + zeta);
  var B = (Math.cos(theta) * Math.cos(obj.topoDec) * Math.cos(obj.topoRA + zeta)) - (Math.sin(theta) * Math.sin(obj.topoDec));
  var C = (Math.sin(theta) * Math.cos(obj.topoDec) * Math.cos(obj.topoRA + zeta)) - (Math.cos(theta) * Math.sin(obj.topoDec));

  var RAJ2000 = Math.atan2(A, B) + z;
  var DecJ2000 = Math.acos(Math.sqrt((A * A) + (B * B)));	// Math.asin(C);
//alert("JNow -> J2000\n"+showRighAscension(obj.topoRA,false)+" -> "+showRighAscension(RAJ2000,false)+"\n"+showDeclination(obj.topoDec,false)+" -> "+showDeclination(DecJ2000,false));
}

// phi is the observer latitude
function getParallactic( obj, phi, hourangle )
{
  if ( ( hourangle == 0.0 ) && ( obj.topoDec == phi ) )	// Undefined at the zenith
    var para = 0.0;
  else
    var para = Math.atan2(Math.sin(hourangle), (Math.tan(phi) * Math.cos(obj.topoDec)) - (Math.sin(obj.topoDec) * Math.cos(hourangle)));

  return para * R2D;
}

function showHMS( value, elmId )
{
  var h = Math.floor(value);
  var m = Math.floor((value - h) * 60);
  var s = (((value - h) * 60) - m) * 60;

  document.getElementById(elmId).innerHTML = h + "<sup>h</sup>" + zero(m) + "<sup>m</sup>" + zeroFixed2(s) + "<sup>s</sup>";
}

function showRighAscension( ra, html )
{
  var rah = ra * R2H;
  var z1 = ((rah - Math.floor(rah)) * 60);
  var z2 = ((z1 - Math.floor(z1)) * 60);

  if ( html == true )
    return Math.floor(rah) + "<sup>h</sup>" + zero(Math.floor(z1)) + "<sup>m</sup>" + zeroFixed2(z2) + "<sup>s</sup>";
  else
    return Math.floor(rah) + "h" + zero(Math.floor(z1)) + "m" + zeroFixed2(z2) + "s";
}

function showDeclination( dec, html )
{
  var decd = Math.abs(dec * R2D);
  z1 = ((decd - Math.floor(decd)) * 60);
  z2 = ((z1 - Math.floor(z1)) * 60);
  if ( dec < 0 )
    var sign = "-";
  else
  {
    if ( html == true )
      var sign = "&nbsp;";
    else
      var sign = " ";
  }

  if ( html == true )
    return sign + Math.floor(decd) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';
  else
    return sign + Math.floor(decd) + "\xB0 " + zero(Math.floor(z1)) + "' " + zeroFixed2(z2) + '"';
}

// Longitude and latitude are in radians
function showEclipticCoordinates( lon, lat, lon_field, lat_field )
{
  var lond = lon * R2D;
  var z1 = ((lond - Math.floor(lond)) * 60);
  var z2 = ((z1 - Math.floor(z1)) * 60);
  document.getElementById(lon_field).innerHTML = Math.floor(lond) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';

  var latd = Math.abs(lat * R2D);
  z1 = ((latd - Math.floor(latd)) * 60);
  z2 = ((z1 - Math.floor(z1)) * 60);
  if ( lat < 0 )
    var sign = "-";
  else
    var sign = "&nbsp;";
  document.getElementById(lat_field).innerHTML = sign + Math.floor(latd) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';
}

// Right ascension and declination are in radians
function showGeocentricEquatorialCoordinates( obj, ra_field, dec_field )
{
  var rah = obj.geoRA * R2H;
  var z1 = ((rah - Math.floor(rah)) * 60);
  var z2 = ((z1 - Math.floor(z1)) * 60);
  document.getElementById(ra_field).innerHTML = Math.floor(rah) + "<sup>h</sup>" + zero(Math.floor(z1)) + "<sup>m</sup>" + zeroFixed2(z2) + "<sup>s</sup>";

  var decd = Math.abs(obj.geoDec * R2D);
  z1 = ((decd - Math.floor(decd)) * 60);
  z2 = ((z1 - Math.floor(z1)) * 60);
  if ( obj.geoDec < 0 )
    var sign = "-";
  else
    var sign = "&nbsp;";
  document.getElementById(dec_field).innerHTML = sign + Math.floor(decd) + "\xB0&nbsp;" + zero(Math.floor(z1)) + "'&nbsp;" + zeroFixed2(z2) + '"';
}

function showApparentDiameterDistance( language, diameter, distEarth, distSun, diameter_field, earth_field, sun_field )
{
  document.getElementById(diameter_field).innerHTML = fixedn(diameter / distEarth, 2) + '"';
  document.getElementById(sun_field).innerHTML = fixedn(distSun, 5) + ((language == "fr") ? "&nbsp;UA" : "&nbsp;AU");
  document.getElementById(earth_field).innerHTML = fixedn(distEarth, 5) + ((language == "fr") ? "&nbsp;UA" : "&nbsp;AU");
}

// Phase of the Moon
function moon_data( language )
{
  var nj = Math.floor(275 * month / 9) - (2 * Math.floor((month + 9) / 12)) + day - 30;
  var fnj = nj / 365;

  pl = new Array(4);
  ph = new Array(4);
  yp = new Array(4);
  yph = new Array(4);

  if (language == "fr")
  {
    ph[0] = "Nouvelle Lune";
    ph[1] = "Premier Quartier";
    ph[2] = "Pleine Lune";
    ph[3] = "Dernier Quartier";
  }
  else
  {
    ph[0] = "New Moon";
    ph[1] = "First Quarter";
    ph[2] = "Full Moon";
    ph[3] = "Last Quarter";
  }

  n = 0;
  for ( var i = 0; i < 5; i++ )
  {
    var pt = moon_phase_approx(n, fnj);
    if ( (pt - jd) > 14 )
    {
      fnj -= 0.0397;
      pl[i] = moon_phase(n, fnj);
      fnj += 0.0397;
    }
    else
      pl[i] = moon_phase(n, fnj);
    n += 0.25;
  }

/*  var count = pl[0];
  if ( count > jd )
    count -= 29.530588853;*/

  yp[0] = pl[0];
  yp[1] = pl[1];
  yp[2] = pl[2];
  yp[3] = pl[3];
  yph[0] = ph[0];
  yph[1] = ph[1];
  yph[2] = ph[2];
  yph[3] = ph[3];
  permutate();

  document.getElementById("dt0").innerHTML = yph[0];
  document.getElementById("dt1").innerHTML = yph[1];
  document.getElementById("dt2").innerHTML = yph[2];
  document.getElementById("dt3").innerHTML = yph[3];

  document.getElementById("pl0").innerHTML = edate(yp[0], language);
  document.getElementById("pl1").innerHTML = edate(yp[1], language);
  document.getElementById("pl2").innerHTML = edate(yp[2], language);
  document.getElementById("pl3").innerHTML = edate(yp[3], language);

   // Moon's age in days
/*  ip = norm((jd - count) / 29.530588853);
  var ag = Math.floor((ip * 29.53058868) * 100) / 100;
  if ( ag < 3 )
  {
    var agh = (Math.floor(ag * 24));
    agm=Math.floor((ag*24-Math.floor(ag*24))*60);
    document.getElementById("age").innerHTML = agh + "h " + zero(agm) + "mn";
  }
  else
  {
    var agj = Math.floor(ag);
    var agh = Math.floor((ag * 24) - (agj * 24));
    var agm = Math.floor((((ag * 24) - (agj * 24)) - agh) * 60);
    document.getElementById("age").innerHTML = agj + "j " + zero(agh) + "h " + zero(agm) + "mn";
  }*/

  riseset(latObs * R2D, -lonObs * R2D, language);
}

function deltaT( )
{
  var dT = 0.0;
  if ( year < 1620 )
  {
    var y = (year - 2000) / 100;
    if ( (year > -391) && (year <= 946) )
      dT = 2177 + (497 * y) + (44.1 * y * y);
    else if ( (year > 948) && (year < 1620) )
      dT = 102 + (102 * y) + (25.3 * y * y);
  }
  else
  {
    var last = eval("last");
    if ( year <= last )
    {
      var dTc = eval("deltaT_" + year);
      var dTcp1 = eval("deltaT_" + (year + 1));
    }
    else
    {
      var dTc = "-";
      var dTcp1 = "-";
    }
    if ( ( dTc != "-" ) && ( dTcp1 != "-" ) )
      dT = dTc + (dTcp1 - dTc) * month / 12;
    else if ( dTc != "-" )
      dT = dTc;
    else
    {
      var y = year + (month - 0.5) / 12;
      if ( ( year >= 2050 ) && ( year < 2150 ) )
        dT = -20.0 + (32.0 * ((y - 1820) * (y - 1820) / 10000.0)) - (0.5628 * (2150 - y));
      else if ( year >= 2150 )
      {
        var u = (year - 1820) / 100.0;
        dT = 20.0 + (32.0 * u * u);
      }
      else
      {
        var u = y - 2000;
        dT = 62.92 + (u * (0.32217 + (u * (0.005589))));
      }
    }
  }

  return dT;
}

// Julian day from the beginning of the year -4712 at noon UT
function julianDay( day, month, year, hour, minute, second )
{
  var y, m, a, b;

  var gregorian = true;
  if ( year < 1582 )
    gregorian = false;
  else if ( year == 1582 )
  {
    if ( ( month < 10 ) || ( ( month == 10 ) && ( day < 15 ) ) )
      gregorian = false;
  }
  if ( month > 2 )
  {
    y = year;
    m = month;
  }
  else
  {
    y = year - 1;
    m = month + 12;
  }

  a = truncate(y / 100);
  if ( gregorian )
    b = 2 - a + truncate(a / 4);
  else
    b = 0.0;
  var jd = truncate(365.25 * (y + 4716)) + truncate(30.6001 * (m + 1)) + day + b - 1524.5;
  jd += (hour + (minute / 60) + (second / 3600)) / 24;

  return jd;
}

// Convert Julian day to calendar date year, month, day (Meeus page 63)
function julianDay2calendarDate( jd )
{
  var z, f, a, alpha, b, c, d, e, mm;

  jd += 0.5;
  z = truncate(jd);
  f = jd - z;

  if ( z < 2299161 )	// 1582 Oct. 15 12:00 UT
    a = z;
  else
  {
    alpha = truncate((z - 1867216.25) / 36524.25);
    a = z + 1 + alpha - truncate(alpha / 4);
  }

  b = a + 1524;
  c = truncate((b - 122.1) / 365.25);
  d = truncate(365.25 * c);
  e = truncate((b - d) / 30.6001);
  mm = truncate((e < 14) ? (e - 1) : (e - 13));

  return new Array(Math.floor((mm > 2) ? (c - 4716) : (c - 4715)), mm, Math.floor(b - d - Math.floor(30.6001 * e) + f));
}

// Convert Julian time to hour, minutes, and seconds
function julianDay2hms( jd )
{
  jd += 0.5;         // Astronomical to civil
  var sec = (jd - truncate(jd)) * 86400.0;

  return new Array(Math.floor(sec / 3600), Math.floor((sec / 60) % 60), sec % 60);
}

function edate( jd, language )
{
  var cDate, cTime;

  jd += (30.0 / (24 * 60 * 60));   // Round to nearest minute
  cDate = julianDay2calendarDate(jd);
  cTime = julianDay2hms(jd);

  if (language == "fr")
    return ("le " + (zero(cDate[2], 2, " ")) + " " + MonthsFr[cDate[1] - 1] + " \xE0 " + (zero(cTime[0], 2, " ")) + "h" + (zero(cTime[1], 2, "0")) + " UTC ");
  else
    return ("on " + (zero(cDate[2], 2, " ")) + " " + MonthsEn[cDate[1] - 1] + " at " + (zero(cTime[0], 2, " ")) + ":" + (zero(cTime[1], 2, "0")) + " UTC ");
}

function permutate( )
{
  if ( yp[0] > yp[1] )
  {
    yp[0] = pl[1];
    yp[1] = pl[2];
    yp[2] = pl[3];
    yp[3] = pl[0];
    yph[0] = ph[1];
    yph[1] = ph[2];
    yph[2] = ph[3];
    yph[3] = ph[0];
  }
  if ( yp[1] > yp[2] )
  {
    yp[0] = pl[2];
    yp[1] = pl[3];
    yp[2] = pl[0];
    yp[3] = pl[1];
    yph[0] = ph[2];
    yph[1] = ph[3];
    yph[2] = ph[0];
    yph[3] = ph[1];
  }
}

function moon_phase_approx( phase, fnj )
{
  var SynMonth = 29.53058868;
  var k = (year + fnj - 1900) * 12.3685;
  k = Math.floor(k + 0.5);
  k += phase;
  var t = (jd - 2415020) / 36525;
  t = k / 1236.85;
  var t2 = t * t;
  var t3 = t2 * t;

  var pt = 2415020.75933 + (SynMonth * k) + (0.0001178 * t2) - (0.000000155 * t3) + 0.00033 * Math.sin((166.56 + 132.87 * t - 0.009173 * t2) * D2R);

  return pt;
}

function moon_phase( phase, fnj )
{
  var SynMonth = 29.53058868;
  var k = (year + fnj - 1900) * 12.3685;
  k = Math.floor(k + 0.5);
  k += phase;
  var t = (jd - 2415020) / 36525;
  t = k / 1236.85;
  var t2 = t * t;
  var t3 = t2 * t;

  var pt = 2415020.75933 + (SynMonth * k) + (0.0001178 * t2) - (0.000000155 * t3) + 0.00033 * Math.sin((166.56 + 132.87 * t - 0.009173 * t2) * D2R);

  var m = 359.2242 + (29.10535608 * k) - (0.0000333 * t2) - (0.00000347 * t3);
  var mprime = 306.0253 + (385.81691806 * k) + (0.0107306 * t2) + (0.00001236 * t3);
  var f = 21.2964 + (390.67050646 * k) - (0.0016528 * t2) - (0.00000239 * t3);

  if ( (phase == 0) || (phase == 0.5) )
  {
    pt += (0.1734 - 0.000393 * t) * Math.sin(m * D2R);
    pt += 0.0021 * Math.sin(2 * m * D2R);
    pt -= 0.4068 * Math.sin(mprime * D2R);
    pt += 0.0161 * Math.sin(2 * mprime * D2R);
    pt -= 0.0004 * Math.sin(3 * mprime * D2R);
    pt += 0.0104 * Math.sin(2 * f * D2R);
    pt -= 0.0051 * Math.sin((m + mprime) * D2R);
    pt -= 0.0074 * Math.sin((m - mprime) * D2R);
    pt += 0.0004 * Math.sin((2 * f + m) * D2R);
    pt -= 0.0004 * Math.sin((2 * f - m) * D2R);
    pt -= 0.0006 * Math.sin((2 * f + mprime) * D2R);
    pt += 0.0010 * Math.sin((2 * f - mprime) * D2R);
    pt += 0.0005 * Math.sin((m + 2 * mprime) * D2R);
  }
  if ( (phase == 0.25) || (phase == 0.75) )
  {
    pt += (0.1721 - 0.0004 * t) * Math.sin(m * D2R);
    pt += 0.0021 * Math.sin(2 * m * D2R);
    pt -= 0.6280 * Math.sin(mprime * D2R);
    pt += 0.0089 * Math.sin(2 * mprime * D2R);
    pt -= 0.0004 * Math.sin(3 * mprime * D2R);
    pt += 0.0079 * Math.sin(2 * f * D2R);
    pt -= 0.0119 * Math.sin((m + mprime) * D2R);
    pt -= 0.0047 * Math.sin((m - mprime) * D2R);
    pt += 0.0003 * Math.sin((2 * f + m) * D2R);
    pt -= 0.0004 * Math.sin((2 * f - m) * D2R);
    pt -= 0.0006 * Math.sin((2 * f + mprime) * D2R);
    pt += 0.0021 * Math.sin((2 * f - mprime) * D2R);
    pt += 0.0003 * Math.sin((m + 2 * mprime) * D2R);
    pt += 0.0004 * Math.sin((m - 2 * mprime) * D2R);
    pt -= 0.0003 * Math.sin((2 * m + mprime) * D2R);
  }
  if ( phase == 0.25 )
    pt += 0.0028 - 0.0004 * Math.cos(m * D2R) + 0.0003 * Math.cos(mprime * D2R);
  else if ( phase == 0.75 )
    pt += -0.0028 + 0.0004 * Math.cos(m * D2R) - 0.0003 * Math.cos(mprime * D2R);

  return pt;
}

// Calculate moonrise and moonset times for the current day (Sky & Telescope, July 1989, page 78)
function riseset( lat, lon, language )
{
  var zone = Math.round(0 / 60);     // UTC
  var jd = julian_day() - 2451545.0;   // Julian day relative to Jan 1, 2000 at noon

  var mp = new Array(3);
  for ( var i = 0; i < 3; i++ )
  {
    mp[i] = new Array(3);
    for ( var j = 0; j < 3; j++ )
      mp[i][j] = 0.0;
  }
  lon /= 360;
  var tz = zone / 24;
  var t0 = lst(lon, jd, tz);        // Local sidereal time
  jd += tz;                         // Moon position at start of day
  for ( var k = 0; k < 3; k++ )
  {
    moon(jd);
    mp[k][0] = Sky[0];
    mp[k][1] = Sky[1];
    mp[k][2] = Sky[2];
    jd += 0.5;
  }
  if ( mp[1][0] <= mp[0][0] )
    mp[1][0] += PIx2;
  if ( mp[2][0] <= mp[1][0] )
    mp[2][0] += PIx2;
  RAn[0] = mp[0][0];
  Decl[0] = mp[0][1];
  Moonrise = false;
  Moonset = false;

  for ( k = 0; k < 24; k++ )      // Check each hour of this day
  {
    ph = (k + 1) / 24;

    RAn[2] = interpolate(mp[0][0], mp[1][0], mp[2][0], ph);
    Decl[2] = interpolate(mp[0][1], mp[1][1], mp[2][1], ph);

    VHz[2] = test_moon(k, zone, t0, lat, mp[1][2]);
    RAn[0] = RAn[2];             // Advance to next hour
    Decl[0] = Decl[2];
    VHz[0] = VHz[2];
  }

  if (language == "fr")
  {
    document.getElementById("moonrise").innerHTML = notn(zero(Rise_time[0])) + "h" + notn(zero(Rise_time[1])) + "m";
    document.getElementById("moonset").innerHTML  = notn(zero(Set_time[0])) + "h" + notn(zero(Set_time[1])) + "m";
  }
  else
  {
    document.getElementById("moonrise").innerHTML = notn(zero(((Rise_time[0] > 12) ? Rise_time[0] - 12 : Rise_time[0]))) + ":" + notn(zero(Rise_time[1])) + ((Rise_time[0] > 12) ? "pm" : "am");
    document.getElementById("moonset").innerHTML  = notn(zero(((Set_time[0] > 12) ? Set_time[0] - 12 : Set_time[0]))) + ":" + notn(zero(Set_time[1])) + ((Set_time[0] > 12) ? "pm" : "am");
  }
  document.getElementById("moonriseAz").innerHTML = azimut = fixedn(Rise_az, 1) + "\xB0";
  document.getElementById("moonsetAz").innerHTML  = azimut = fixedn(Set_az, 1) + "\xB0";
  special_message(language);
}

// Local Sidereal Time
function lst( lon, jd2000, z )
{
  var t = jd2000 / 36525.0;
  var s = 24110.54841 + (t * (8640184.812866 + (t * (0.093104 - (t * 0.0000062))))) + (86400.0 * ((INVERSE_SIDEREAL_DAY * z) + lon));
  s /= 86400;
  s -= Math.floor(s);

  return s * 360 * D2R;
}

// 3-point interpolation
function interpolate( f0, f1, f2, p )
{
  var a = f1 - f0;
  var b = f2 - f1 - a;
  var f = f0 + p * ((2 * a) + (b * ((2 * p) - 1)));

  return f;
}

// Test an hour for an event
function test_moon( k, zone, t0, lat, plx )
{
  var ha = [0.0, 0.0, 0.0];
  var a, b, d, e, z;
  var min, time;
  var az, hz, nz, dz;

  if ( RAn[2] < RAn[0] )
    RAn[2] += PIx2;

  ha[0] = t0 - RAn[0] + (k * K1);
  ha[2] = t0 - RAn[2] + (k * K1) + K1;

  ha[1] = (ha[2] + ha[0]) / 2;          // Hour angle at half hour
  Decl[1] = (Decl[2] + Decl[0]) / 2;        // Declination at half hour
  var s = Math.sin(D2R * lat);
  var c = Math.cos(D2R * lat);
   // Refraction + sun semidiameter at horizon + parallax correction
  z = Math.cos(D2R * (90.567 - (41.685 / plx)));
  if ( k <= 0 )                         // First call
    VHz[0] = (s * Math.sin(Decl[0])) + (c * Math.cos(Decl[0]) * Math.cos(ha[0])) - z;
  VHz[2] = (s * Math.sin(Decl[2])) + (c * Math.cos(Decl[2]) * Math.cos(ha[2])) - z;

  if ( sgn(VHz[0]) == sgn(VHz[2]) )
    return VHz[2];                      // No event this hour

  VHz[1] = (s * Math.sin(Decl[1])) + (c * Math.cos(Decl[1]) * Math.cos(ha[1])) - z;
  a = (2 * VHz[2]) - (4 * VHz[1]) + (2 * VHz[0]);
  b = (4 * VHz[1]) - (3 * VHz[0]) - VHz[2];
  d = (b * b) - (4 * a * VHz[0]);
  if ( d < 0 )
    return VHz[2];                      // No event this hour

  d = Math.sqrt(d);
  e = (-b + d) / (2 * a);
  if ( ( e > 1 ) || ( e < 0 ) )
    e = (-b - d) / (2 * a);
  time = k + e + (1 / 120);             // Time of an event + round up
  var hr = Math.floor(time);
  min = Math.floor((time - hr) * 60);
  hz = ha[0] + (e * (ha[2] - ha[0]));   // Azimuth of the moon at the event
  nz = -Math.cos(Decl[1]) * Math.sin(hz);
  dz = (c * Math.sin(Decl[1])) - (s * Math.cos(Decl[1]) * Math.cos(hz));
  az = Math.atan2(nz, dz) * R2D;
  if ( az < 0 )
    az += 360;

  if ( (VHz[0] < 0) && (VHz[2] > 0) )
  {
    Rise_time[0] = hr;
    Rise_time[1] = min;
    Rise_az = az;
    Moonrise = true;
  }

  if ( (VHz[0] > 0) && (VHz[2] < 0) )
  {
    Set_time[0] = hr;
    Set_time[1] = min;
    Set_az = az;
    Moonset = true;
  }

  return VHz[2];
}

// Check for no moonrise and/or no moonset
function special_message( language )
{
  if ( (!Moonrise) && (!Moonset) )  // Neither moonrise nor moonset
  {
    if (VHz[2] < 0)
    {
      if (language == "fr")
        document.getElementById("moonmsg").innerHTML = "La Lune est sous l'horizon toute la journ\xE9e";
      else
        document.getElementById("moonmsg").innerHTML = "The Moon is under the horizon the whole day";
      document.getElementById("moonrise").innerHTML = "---";
      document.getElementById("moonset").innerHTML = "---";
      document.getElementById("moonriseAz").innerHTML = "---";
      document.getElementById("moonsetAz").innerHTML = "---";
    }
    else
    {
      if (language == "fr")
        document.getElementById("moonmsg").innerHTML = "La Lune est visible toute la journ\xE9e";
      else
        document.getElementById("moonmsg").innerHTML = "The Moon is up the whole day";
      document.getElementById("moonrise").innerHTML = "---";
      document.getElementById("moonset").innerHTML = "---";
      document.getElementById("moonriseAz").innerHTML = "---";
      document.getElementById("moonsetAz").innerHTML = "---";
    }
  }
  else                              // Moonrise or moonset
  {
    if ( !Moonrise )
    {
      if (language == "fr")
        document.getElementById("moonmsg").innerHTML = "Pas de lever de Lune \xE0 cette date";
      else
        document.getElementById("moonmsg").innerHTML = "No Moonrise on this date";
      document.getElementById("moonrise").innerHTML = "---";
      document.getElementById("moonriseAz").innerHTML ="---";
    }
    else if ( !Moonset )
    {
      if (language == "fr")
        document.getElementById("moonmsg").innerHTML  = "Pas de coucher de Lune \xE0 cette date";
      else
        document.getElementById("moonmsg").innerHTML  = "No Moonset on this date";
      document.getElementById("moonset").innerHTML = "---";
      document.getElementById("moonsetAz").innerHTML  = "---";
    }
    else
      document.getElementById("moonmsg").innerHTML = "";
  }
}

// Moon's position using fundamental arguments (Van Flandern & Pulkkinen, 1979)
function moon( jd )
{
  var d, f, g, h, m, n, s, u, v, w;

  h = 0.606434 + 0.03660110129 * jd;
  m = 0.374897 + 0.03629164709 * jd;
  f = 0.259091 + 0.0367481952 * jd;
  d = 0.827362 + 0.03386319198 * jd;
  n = 0.347343 - 0.00014709391 * jd;
  g = 0.993126 + 0.0027377785 * jd;
  h -= Math.floor(h);
  m -= Math.floor(m);
  f -= Math.floor(f);
  d -= Math.floor(d);
  n -= Math.floor(n);
  g -= Math.floor(g);
  h *= PIx2;
  m *= PIx2;
  f *= PIx2;
  d *= PIx2;
  n *= PIx2;
  g *= PIx2;
  v = 0.39558 * Math.sin(f + n);
  v += 0.082  * Math.sin(f);
  v += 0.03257 * Math.sin(m - f - n);
  v += 0.01092 * Math.sin(m + f + n);
  v += 0.00666 * Math.sin(m - f);
  v -= 0.00644 * Math.sin(m + f - 2 * d + n);
  v -= 0.00331 * Math.sin(f - 2 * d + n);
  v -= 0.00304 * Math.sin(f - 2 * d);
  v -= 0.0024 * Math.sin(m - f - 2 * d - n);
  v += 0.00226 * Math.sin(m + f);
  v -= 0.00108 * Math.sin(m + f - 2 * d);
  v -= 0.00079 * Math.sin(f - n);
  v += 0.00078 * Math.sin(f + 2 * d + n);

  u = 1 - 0.10828 * Math.cos(m);
  u -= 0.0188 * Math.cos(m - 2 * d);
  u -= 0.01479 * Math.cos(2 * d);
  u += 0.00181 * Math.cos(2 * m - 2 * d);
  u -= 0.00147 * Math.cos(2 * m);
  u -= 0.00105 * Math.cos(2 * d - g);
  u -= 0.00075 * Math.cos(m - 2 * d + g);

  w = 0.10478 * Math.sin(m);
  w -= 0.04105 * Math.sin(2 * f + 2 * n);
  w -= 0.0213 * Math.sin(m - 2 * d);
  w -= 0.01779 * Math.sin(2 * f + n);
  w += 0.01774 * Math.sin(n);
  w += 0.00987 * Math.sin(2 * d);
  w -= 0.00338 * Math.sin(m - 2 * f - 2 * n);
  w -= 0.00309 * Math.sin(g);
  w -= 0.0019 * Math.sin(2 * f);
  w -= 0.00144 * Math.sin(m + n);
  w -= 0.00144 * Math.sin(m - 2 * f - n);
  w -= 0.00113 * Math.sin(m + 2 * f + 2 * n);
  w -= 0.00094 * Math.sin(m - 2 * d + g);
  w -= 0.00092 * Math.sin(2 * m - 2 * d);
  s = w / Math.sqrt(u - (v * v));          // Moon's right ascension
  Sky[0] = h + Math.atan(s / Math.sqrt(1 - (s * s)));
  s = v / Math.sqrt(u);                    // Moon's declination
  Sky[1] = Math.atan(s / Math.sqrt(1 - (s * s)));
  Sky[2] = 60.40974 * Math.sqrt(u);        // Moon's parallax
}

// Determine the Julian day from the calendar date (Jean Meeus, "Astronomical Algorithms", Willmann-Bell, 1991)
function julian_day( )
{
  var y, m, a, b, jd;

  var gregorian = true;
  if ( year < 1582 )
    gregorian = false;
  else if ( year == 1582 )
  {
    if ( ( month < 10 ) || ( ( month == 10 ) && ( day < 15 ) ) )
      gregorian = false;
  }
  if ( month > 2 )
  {
    y = year;
    m = month;
  }
  else
  {
    y = year - 1;
    m = month + 12;
  }

  a = Math.floor(y / 100);
  if ( gregorian )
    b = 2 - a + Math.floor(a / 4);
  else
    b = 0.0;
  jd = Math.floor(365.25 * (y + 4716)) + Math.floor(30.6001 * (m + 1)) + day + b - 1524.5;

  return jd;
}

// Account for the horizontal parallax (see http://astro.if.ufrgs.br/trigesf/position.html#13)
function geocentric2topocentricAltAz( alt_geoc, dist )
{
  var corr = Math.atan(Math.cos(alt_geoc * D2R) / (dist - Math.sin(alt_geoc * D2R)));
  var alt_topoc = alt_geoc - (corr * R2D);

  return alt_topoc;
}

// Geocentric equatorial coordinates
function geocentricEquatorial( obj, obliquity, l, beta )
{
   // 0 to 2Pi radians, i.e. 0 to 360 degrees or 0 to 24 hours
  obj.geoRA = Math.atan((Math.cos(obliquity) * Math.sin(l) - Math.tan(beta) * Math.sin(obliquity)) / Math.cos(l));
  if ( obj.geoRA < 0 )
    obj.geoRA += PIx2;
  if ( Math.cos(l) < 0 )
    obj.geoRA += Math.PI;
  if ( obj.geoRA > PIx2 )
    obj.geoRA -= PIx2;

   // +/-Pi/2 radians, i.e. +/-90 degrees
  obj.geoDec = Math.asin(Math.sin(beta) * Math.cos(obliquity) + Math.cos(beta) * Math.sin(obliquity) * Math.sin(l));
}

// Geocentric to topocentric equatorial coordinates (Meeus AA page 279)
// elevation in meters, dist in kilometers, longitude positive to the West
function geocentric2topocentricEquatorial( obj, lat, lon, elevation, dist, jd, jde, moon )
{
  if ( moon )
    var sinPi = ER / dist;	// For the Moon
  else
    var sinPi = Math.sin(SUNPARALLAX * D2R / 3600.0) / dist;

  var u = Math.atan2((1.0 - flattening) * Math.tan(lat), 1.0);
  var rhosinphi = ((1.0 - flattening) * Math.sin(u)) + (elevation / (ER * 1000.0) * Math.sin(lat));
  var rhocosphi = Math.cos(u) + (elevation / (ER * 1000.0) * Math.cos(lat));

//  var GMST = GreenwichMeanSiderealTime(jd);
//  var nutationRA_corr = nutation_RA(jde);
  var theta0 = (GMST * 15) + nutationRA_corr;	// Apparent sidereal time (Greenwich hour angle)
  var H = theta0 - (lon * R2D) - (obj.geoRA * R2D);
  H = rev(H) * D2R;

  var deltaAlpha = Math.atan2(-rhocosphi * sinPi * Math.sin(H), (Math.cos(obj.geoDec) - (rhocosphi * sinPi * Math.cos(H))));

  var deltaP = Math.atan2(((Math.sin(obj.geoDec) - (rhosinphi * sinPi)) * Math.cos(deltaAlpha)), (Math.cos(obj.geoDec) - (rhocosphi * sinPi * Math.cos(H))));

  obj.topoRA = obj.geoRA + deltaAlpha;
  if ( obj.topoRA < 0 )
    obj.topoRA += PIx2;
  else if ( obj.topoRA > PIx2 )
    obj.topoRA -= PIx2;
  obj.topoDec = deltaP;
}

// Nutation in right ascension or equation of the equinoxes
function nutation_RA( jd )
{
   // Epoch J2000: 2000 January 1 at noon UT
  var T_J2000 = (jd - 2451545.0) / 36525.0;
  var T2_J2000 = T_J2000 * T_J2000;
  var T3_J2000 = T2_J2000 * T_J2000;
  var T4_J2000 = T2_J2000 * T2_J2000;

   // Mean elongation of the Moon from the Sun
  var d = rev(297.85036 + (445267.111480 * T_J2000) - (0.0019142 * T2_J2000) + (T3_J2000 / 189474.0)) * D2R;
   // Mean anomaly of the Sun (Earth)
  var m = rev(357.52772 + (35999.050340 * T_J2000) - (0.0001603 * T2_J2000) - (T3_J2000 / 300000.0)) * D2R;
   // Mean anomaly of the Moon
  var n = rev(134.96298 + (477198.867398 * T_J2000) + (0.0086972 * T2_J2000) + (T3_J2000 / 56250.0)) * D2R;
   // Moon's argument of latitude
  var f = rev(93.27191 + (483202.017538 * T_J2000) - (0.0036825 * T2_J2000) + (T3_J2000 / 327270.0)) * D2R;
   // Longitude of ascending node
  var om = rev(125.04452 - (1934.136261 * T_J2000) + (0.0020708 * T2_J2000) + (T3_J2000 / 450000.0)) * D2R;

  var nu = (-171996 - 174.2 * T_J2000) * Math.sin(om);
  nu += (-13187 - 1.6 * T_J2000) * Math.sin(-2 * d + 2 * f + 2 * om);
  nu += (-2274 - 0.2 * T_J2000) * Math.sin(2 * f + 2 * om);
  nu += (2062 + 0.2 * T_J2000) * Math.sin(2 * om);
  nu += (1426 - 3.4 * T_J2000) * Math.sin(m);
  nu += (712 + 0.1 * T_J2000) * Math.sin(n);
  nu += (-517 + 1.2 * T_J2000) * Math.sin(-2 * d + m + 2 * f + 2 * om);
  nu += (-386 - 0.4 * T_J2000) * Math.sin(2 * f + om);
  nu -= 301 * Math.sin(n + 2 * f + 2 * om);
  nu += (217 - 0.5 * T_J2000) * Math.sin(-2 * d - m + 2 * f + 2 * om);
  nu -= 158 * Math.sin(-2 * d + n);
  nu += (129 + 0.1 * T_J2000) * Math.sin(-2 * d + 2 * f + om);
  nu += 123  * Math.sin(-n + 2 * f + 2 * om);
  nu += 63 * Math.sin(2 * d);
  nu += (63 + 0.1 * T_J2000) * Math.sin(n + om);
  nu -= 59 * Math.sin(2 * d - n + 2 * f + 2 * om);
  nu -= (58 + 0.1 * T_J2000) * Math.sin(-n + om);
  nu -= 51 * Math.sin(n + 2 * f + om);
  nu += 48 * Math.sin(-2 * d + 2 * n);
  nu += 46 * Math.sin(-2 * n + 2 * f + om);
  nu -= 38 * Math.sin(2 * (d + f + om));
  nu -= 31 * Math.sin(2 * (n + f + om));
  nu += 29 * Math.sin(2 * n);
  nu += 29 * Math.sin(-2 * d + n + 2 * f + 2 * om);
  nu += 26 * Math.sin(2 * f);
  nu -= 22 * Math.sin(-2 * d + 2 * f);
  nu += 21 * Math.sin(-n + 2 * f + om);
  nu += (17 - 0.1 * T_J2000) * Math.sin(2 * m);
  nu += 16 * Math.sin(2 * d - n + om);
  nu += (-16 + 0.1 * T_J2000) * Math.sin(-2 * d + 2 * m + 2 * f + 2 * om);
  nu -= 15 * Math.sin(m + om);
  nu -= 13 * Math.sin(-2 * d + n + om);
  nu -= 12 * Math.sin(-m + om);
  nu += 11 * Math.sin(2 * n - 2 * f);
  nu -= 10 * Math.sin(2 * d - n + 2 * f + om);
  nu -= 8 * Math.sin(2 * d + n + 2 * f + 2 * om);
  nu += 7 * Math.sin(m + 2 * f + 2 * om);
  nu -= 7 * Math.sin(-2 * d + m + n);
  nu -= 7 * Math.sin(-m + 2 * f + 2 * om);
  nu -= 7 * Math.sin(2 * d + 2 * f + om);
  nu += 6 * Math.sin(2 * d + n);
  nu += 6 * Math.sin(-2 * d + 2 * n + 2 * f + 2 * om);
  nu += 6 * Math.sin(-2 * d + n + 2 * f + om);
  nu -= 6 * Math.sin(2 * d - 2 * n + om);
  nu -= 6 * Math.sin(2 * d + om);
  nu += 5 * Math.sin(-m + n);
  nu -= 5 * Math.sin(-2 * d - m + 2 * f + om);
  nu -= 5 * Math.sin(-2 * d + om);
  nu -= 5 * Math.sin(2 * n + 2 * f + om);
  nu += 4 * Math.sin(-2 * d + 2 * n + om);
  nu += 4 * Math.sin(-2 * d + m + 2 * f + om);
  nu += 4 * Math.sin(n - 2 * f);
  nu -= 4 * Math.sin(-d + n);
  nu -= 4 * Math.sin(-2 * d + m);
  nu -= 4 * Math.sin(d);
  nu += 3 * Math.sin(n + 2 * f);
  nu -= 3 * Math.sin(-2 * n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(-d - m + n);
  nu -= 3 * Math.sin(m + n);
  nu -= 3 * Math.sin(-m + n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(2 * d - m - n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(3 * n + 2 * f + 2 * om);
  nu -= 3 * Math.sin(2 * d - m + 2 * f + 2 * om);
  var nutation_longitude = nu / 10000;
  nu = (92025 + 8.9 * T_J2000) * Math.cos(om);
  nu += (5736 - 3.1 * T_J2000) * Math.cos(-2 * d + 2 * f + 2 * om);
  nu += (977 - 0.5 * T_J2000) * Math.cos(2 * f + 2 * om);
  nu += (-895 + 0.5 * T_J2000) * Math.cos(2 * om);
  nu += (54 - 0.1 * T_J2000) * Math.cos(m);
  nu -= 7 * Math.cos(n);
  nu += (224 - 0.6 * T_J2000) * Math.cos(-2 * d + m + 2 * f + 2 * om);
  nu += 200 * Math.cos(2 * f + om);
  nu += (129 - 0.1 * T_J2000) * Math.cos(n + 2 * f + 2 * om);
  nu += (-95 + 0.3 * T_J2000) * Math.cos(-2 * d - m + 2 * f + 2 * om);
  nu -= 70 * Math.cos(-2 * d + 2 * f + om);
  nu -= 53 * Math.cos(-n + 2 * f + 2 * om);
  nu -= 33 * Math.cos(n + om);
  nu += 26 * Math.cos(2 * d - n + 2 * f + 2 * om);
  nu += 32 * Math.cos(-n + om);
  nu += 27 * Math.cos(n + 2 * f + om);
  nu -= 24 * Math.cos(-2 * n + 2 * f + om);
  nu += 16 * Math.cos(2 * (d + f + om));
  nu += 13 * Math.cos(2 * (n + f + om));
  nu -= 12 * Math.cos(-2 * d + n + 2 * f + 2 * om);
  nu -= 10 * Math.cos(-n + 2 * f + om);
  nu -= 8 * Math.cos(2 * d - n + om);
  nu += 7 * Math.cos(-2 * d + 2 * m + 2 * f + 2 * om);
  nu += 9 * Math.cos(m + om);
  nu += 7 * Math.cos(-2 * d + n + om);
  nu += 6 * Math.cos(-m + om);
  nu += 5 * Math.cos(2 * d - n + 2 * f + om);
  nu += 3 * Math.cos(2 * d + n + 2 * f + 2 * om);
  nu -= 3 * Math.cos(m + 2 * f + 2 * om);
  nu += 3 * Math.cos(-m + 2 * f + 2 * om);
  nu += 3 * Math.cos(2 * d + 2 * f + om);
  nu -= 3 * Math.cos(-2 * d + 2 * n + 2 * f + 2 * om);
  nu -= 3 * Math.cos(-2 * d + n + 2 * f + om);
  nu += 3 * Math.cos(2 * d - 2 * n + om);
  nu += 3 * Math.cos(2 * d + om);
  nu += 3 * Math.cos(-2 * d - m + 2 * f + om);
  nu += 3 * Math.cos(-2 * d + om);
  nu += 3 * Math.cos(2 * n + 2 * f + om);
  var nutation_obliquity = nu / 10000;
//  var obliquity = (23 + (26 / 60) + (21.448 / 3600) - ((46.815 / 3600) * T_J2000) - ((0.00059 / 3600) * T2_J2000) + ((0.001813 / 3600) * T3_J2000)) + (nutation_obliquity / 3600);	// Less accurate
  var U = T_J2000 / 100.0;
  var laskar = U * (-4680.93 + (U * (-1.55 + (U * (1999.25 + (U * (-51.38 + (U * (-249.67 + (U * (-39.05 + (U * (7.12 + (U * (27.87 + (U * (5.79 + (U * 2.45))))))))))))))))));
  var obliquity = 23 + (26 / 60) + (21.448 / 3600) + (laskar / 3600) + (nutation_obliquity / 3600);	// More accurate
  var nutation_corr = (nutation_longitude / 3600) * Math.cos(obliquity * D2R);

  return nutation_corr;
}

// Calculate the Range of Google Earth's LookAt element (see http://code.google.com/intl/fr/apis/kml/documentation/kmlsky.html)
// fov is in arc seconds
function geRange( fov )
{
  var beta = fov / 2;

  return (ER * 1000) * ((1.1917536 * Math.sin(beta)) - Math.cos(beta) + 1);
}

function GreenwichMeanSiderealTime( jd )
{
   // Greenwich mean sidereal time at 00:00 UT (GMST0)
/*  var J0 = Math.floor(jd + 0.5) - 0.5;  // Julian day at 00:00 UT
  var T0 = (J0 - 2451545.0) / 36525.0;
  var MST0 = 100.46061837 + (T0 * (36000.770053608 + (T0 * (0.000387933 - (T0 / 38710000.0)))));
  MST0 = rev(MST0);
  var GMST0 = MST0 / 15.0; // Greenwich mean sidereal time at 00:00 UT (without the nutation correction)*/

   // Greenwich mean sidereal time at a given hour (GMST)
/*  var st = hour + (minute / 60) + (second / 3600);
  var GMST = GMST0 + (st * INVERSE_SIDEREAL_DAY);*/
  var T = (jd - 2451545.0) / 36525.0;
  var GMST = 280.46061837 + (360.98564736629 * (jd - 2451545.0)) + (T * T * (0.000387933 - (T / 38710000.0)));
  GMST = rev(GMST) / 15.0;
  if ( GMST >= 24 )
    GMST -= 24;

  return GMST;
}

function zero( value )
{
  if ( value < 10 )
  {
    if ( value < 0 )
      value = "-0" + Math.abs(value);
    else
      value = "0" + value;
  }
  else
    value = "" + value;

  return ( myLanguage == "fr" ) ? value.replace(/\./, ',') : value;
}

function zeroFixed2( value )
{
  value = value.toFixed(2);
  if ( value < 10 )
    value = "0" + value;
  else
    value = "" + value;

  return ( myLanguage == "fr" ) ? value.replace(/\./, ',') : value;
}

function fixedn( value, n )
{
  value = value.toFixed(n);

  return ( myLanguage == "fr" ) ? ("" + value).replace(/\./, ',') : value;
}

function norm( x )
{
  x -= Math.floor(x);
  if ( x < 0 )
    x += 1;

  return x;
}

function mod2pi( x )
{
  var bo = x / PIx2;
  var ao = PIx2 * (bo - truncate(bo));
  if ( ao < 0.0 )
    ao += PIx2;

  return ao;
}

function notn( x )
{
  if ( isNaN(x) )
    x = " -- ";

  return x;
}

function aff( x )
{
  if ( x > 20 )
    x = " -- ";
  else
  {
    x = x.toFixed(2);
    x = ( myLanguage == "fr" ) ? ("" + x).replace(/\./, ',') : x;
    x += "\xB0";
  }

  return x;
}

function truncate( x )
{
  return ( x >= 0.0 ) ? Math.floor(x) : Math.ceil(x);
}

// Returns value for sign of argument
function sgn( x )
{
  var rv;
  if ( x > 0.0 )
    rv =  1;
  else if ( x < 0.0 )
    rv = -1;
  else
    rv = 0;

  return rv;
}

//
// Return an angle between 0 and 360 degrees
function rev( angle )
{
  return angle - (360.0 * Math.floor(angle / 360.0));
}

//
// Return an angle between 0 and 2PI radians
function revrad( angle )
{
  return angle - (PIx2 * Math.floor(angle / PIx2));
}
//-->