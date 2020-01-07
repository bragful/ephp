-module(ephp_timezone).
-author('manuel@altenwald.com').

-include("ephp.hrl").

-export([
    timezone_to_abbreviation/2,
    abbreviations/0,
    timezone_info/1,
    is_valid/1
]).

-type timezone() :: binary().
-type dst() :: boolean().
-type offset() :: integer().
-type abbreviation() :: binary().
-type timezone_info() :: {dst(), offset(), abbreviation()}.

-spec is_valid(timezone()) -> boolean().

is_valid(TZ) ->
    case lists:member(TZ, abbreviations()) of
        true -> true;
        false -> timezone_to_abbreviation(true, TZ) =/= TZ
    end.

-spec timezone_to_abbreviation(dst(), timezone()) -> abbreviation().

timezone_to_abbreviation(true, <<"Australia/Adelaide">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"Australia/Broken_Hill">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"Australia/Darwin">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"Australia/North">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"Australia/South">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"Australia/Yancowinna">>) -> <<"ACDT">>;
timezone_to_abbreviation(true, <<"America/Porto_Acre">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/Adelaide">>) -> <<"ACST">>;
timezone_to_abbreviation(true, <<"America/Eirunepe">>) -> <<"ACST">>;
timezone_to_abbreviation(true, <<"America/Rio_Branco">>) -> <<"ACST">>;
timezone_to_abbreviation(true, <<"Brazil/Acre">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Asia/Jayapura">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/Broken_Hill">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/Darwin">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/North">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/South">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"Australia/Yancowinna">>) -> <<"ACST">>;
timezone_to_abbreviation(false, <<"America/Porto_Acre">>) -> <<"ACT">>;
timezone_to_abbreviation(false, <<"America/Eirunepe">>) -> <<"ACT">>;
timezone_to_abbreviation(false, <<"America/Rio_Branco">>) -> <<"ACT">>;
timezone_to_abbreviation(false, <<"Brazil/Acre">>) -> <<"ACT">>;
timezone_to_abbreviation(true, <<"Australia/Eucla">>) -> <<"ACWDT">>;
timezone_to_abbreviation(false, <<"Australia/Eucla">>) -> <<"ACWST">>;
timezone_to_abbreviation(true, <<"America/Goose_Bay">>) -> <<"ADDT">>;
timezone_to_abbreviation(true, <<"America/Pangnirtung">>) -> <<"ADDT">>;
timezone_to_abbreviation(false, <<"Africa/Addis_Ababa">>) -> <<"ADMT">>;
timezone_to_abbreviation(false, <<"Africa/Asmara">>) -> <<"ADMT">>;
timezone_to_abbreviation(false, <<"Africa/Asmera">>) -> <<"ADMT">>;
timezone_to_abbreviation(true, <<"America/Halifax">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Barbados">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Blanc-Sablon">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Glace_Bay">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Martinique">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Moncton">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"America/Thule">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"Atlantic/Bermuda">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"Canada/Atlantic">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"Asia/Baghdad">>) -> <<"ADT">>;
timezone_to_abbreviation(true, <<"Australia/Melbourne">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Antarctica/Macquarie">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/ACT">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Brisbane">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Canberra">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Currie">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Hobart">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Lindeman">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/NSW">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Queensland">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Sydney">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Tasmania">>) -> <<"AEDT">>;
timezone_to_abbreviation(true, <<"Australia/Victoria">>) -> <<"AEDT">>;
timezone_to_abbreviation(false, <<"Australia/Melbourne">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Antarctica/Macquarie">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/ACT">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Brisbane">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Canberra">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Currie">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Hobart">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/LHI">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Lindeman">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Lord_Howe">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/NSW">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Queensland">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Sydney">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Tasmania">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Australia/Victoria">>) -> <<"AEST">>;
timezone_to_abbreviation(false, <<"Asia/Kabul">>) -> <<"AFT">>;
timezone_to_abbreviation(true, <<"America/Anchorage">>) -> <<"AHDT">>;
timezone_to_abbreviation(false, <<"America/Anchorage">>) -> <<"AHST">>;
timezone_to_abbreviation(false, <<"America/Adak">>) -> <<"AHST">>;
timezone_to_abbreviation(false, <<"America/Atka">>) -> <<"AHST">>;
timezone_to_abbreviation(true, <<"America/Juneau">>) -> <<"AKDT">>;
timezone_to_abbreviation(true, <<"America/Nome">>) -> <<"AKDT">>;
timezone_to_abbreviation(true, <<"America/Sitka">>) -> <<"AKDT">>;
timezone_to_abbreviation(true, <<"America/Yakutat">>) -> <<"AKDT">>;
timezone_to_abbreviation(false, <<"America/Juneau">>) -> <<"AKST">>;
timezone_to_abbreviation(false, <<"America/Nome">>) -> <<"AKST">>;
timezone_to_abbreviation(false, <<"America/Sitka">>) -> <<"AKST">>;
timezone_to_abbreviation(false, <<"America/Yakutat">>) -> <<"AKST">>;
timezone_to_abbreviation(true, <<"Asia/Aqtobe">>) -> <<"AKTST">>;
timezone_to_abbreviation(false, <<"Asia/Aqtobe">>) -> <<"AKTT">>;
timezone_to_abbreviation(true, <<"Asia/Almaty">>) -> <<"ALMST">>;
timezone_to_abbreviation(false, <<"Asia/Almaty">>) -> <<"ALMT">>;
timezone_to_abbreviation(true, <<"Asia/Yerevan">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Boa_Vista">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Campo_Grande">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Cuiaba">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Manaus">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Porto_Velho">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"America/Santarem">>) -> <<"AMST">>;
timezone_to_abbreviation(true, <<"Brazil/West">>) -> <<"AMST">>;
timezone_to_abbreviation(false, <<"Asia/Yerevan">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Asuncion">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Boa_Vista">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Campo_Grande">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Cuiaba">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Manaus">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Porto_Velho">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"America/Santarem">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"Brazil/West">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"Europe/Amsterdam">>) -> <<"AMT">>;
timezone_to_abbreviation(false, <<"Europe/Athens">>) -> <<"AMT">>;
timezone_to_abbreviation(true, <<"Asia/Anadyr">>) -> <<"ANAST">>;
timezone_to_abbreviation(false, <<"Asia/Anadyr">>) -> <<"ANAT">>;
timezone_to_abbreviation(false, <<"America/Curacao">>) -> <<"ANT">>;
timezone_to_abbreviation(false, <<"America/Aruba">>) -> <<"ANT">>;
timezone_to_abbreviation(false, <<"America/Kralendijk">>) -> <<"ANT">>;
timezone_to_abbreviation(false, <<"America/Lower_Princes">>) -> <<"ANT">>;
timezone_to_abbreviation(false, <<"Africa/Luanda">>) -> <<"AOT">>;
timezone_to_abbreviation(true, <<"America/Puerto_Rico">>) -> <<"APT">>;
timezone_to_abbreviation(true, <<"Asia/Aqtau">>) -> <<"AQTST">>;
timezone_to_abbreviation(false, <<"Asia/Aqtau">>) -> <<"AQTT">>;
timezone_to_abbreviation(true, <<"America/Buenos_Aires">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Buenos_Aires">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Catamarca">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/ComodRivadavia">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Cordoba">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Jujuy">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/La_Rioja">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Mendoza">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Rio_Gallegos">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Salta">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/San_Juan">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/San_Luis">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Tucuman">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Argentina/Ushuaia">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Catamarca">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Cordoba">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Jujuy">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Mendoza">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"America/Rosario">>) -> <<"ARST">>;
timezone_to_abbreviation(true, <<"Antarctica/Palmer">>) -> <<"ARST">>;
timezone_to_abbreviation(false, <<"America/Buenos_Aires">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Buenos_Aires">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Catamarca">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/ComodRivadavia">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Cordoba">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Jujuy">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/La_Rioja">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Mendoza">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Rio_Gallegos">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Salta">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/San_Juan">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/San_Luis">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Tucuman">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Argentina/Ushuaia">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Catamarca">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Cordoba">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Jujuy">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Mendoza">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"America/Rosario">>) -> <<"ART">>;
timezone_to_abbreviation(false, <<"Antarctica/Palmer">>) -> <<"ART">>;
timezone_to_abbreviation(true, <<"Asia/Ashkhabad">>) -> <<"ASHST">>;
timezone_to_abbreviation(true, <<"Asia/Ashgabat">>) -> <<"ASHST">>;
timezone_to_abbreviation(false, <<"Asia/Ashkhabad">>) -> <<"ASHT">>;
timezone_to_abbreviation(false, <<"Asia/Ashgabat">>) -> <<"ASHT">>;
timezone_to_abbreviation(false, <<"Asia/Riyadh">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Anguilla">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Antigua">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Barbados">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Blanc-Sablon">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Dominica">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Glace_Bay">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Goose_Bay">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Grenada">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Guadeloupe">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Halifax">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Marigot">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Martinique">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Miquelon">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Moncton">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Montserrat">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Pangnirtung">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Port_of_Spain">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Puerto_Rico">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Santo_Domingo">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/St_Barthelemy">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/St_Kitts">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/St_Lucia">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/St_Thomas">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/St_Vincent">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Thule">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Tortola">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"America/Virgin">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Atlantic/Bermuda">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Canada/Atlantic">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Asia/Aden">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Asia/Baghdad">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Asia/Bahrain">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Asia/Kuwait">>) -> <<"AST">>;
timezone_to_abbreviation(false, <<"Asia/Qatar">>) -> <<"AST">>;
timezone_to_abbreviation(true, <<"Australia/Perth">>) -> <<"AWDT">>;
timezone_to_abbreviation(true, <<"Australia/West">>) -> <<"AWDT">>;
timezone_to_abbreviation(false, <<"Australia/Perth">>) -> <<"AWST">>;
timezone_to_abbreviation(false, <<"Antarctica/Casey">>) -> <<"AWST">>;
timezone_to_abbreviation(false, <<"Australia/West">>) -> <<"AWST">>;
timezone_to_abbreviation(true, <<"Atlantic/Azores">>) -> <<"AZOMT">>;
timezone_to_abbreviation(false, <<"Atlantic/Azores">>) -> <<"AZOT">>;
timezone_to_abbreviation(true, <<"Asia/Baku">>) -> <<"AZST">>;
timezone_to_abbreviation(false, <<"Asia/Baku">>) -> <<"AZT">>;
timezone_to_abbreviation(true, <<"Europe/London">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Asia/Dacca">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Asia/Dhaka">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Europe/Belfast">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Europe/Gibraltar">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Europe/Guernsey">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Europe/Isle_of_Man">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"Europe/Jersey">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"GB">>) -> <<"BDST">>;
timezone_to_abbreviation(true, <<"America/Adak">>) -> <<"BDT">>;
timezone_to_abbreviation(false, <<"Asia/Dacca">>) -> <<"BDT">>;
timezone_to_abbreviation(true, <<"America/Atka">>) -> <<"BDT">>;
timezone_to_abbreviation(false, <<"Asia/Dhaka">>) -> <<"BDT">>;
timezone_to_abbreviation(false, <<"Africa/Mogadishu">>) -> <<"BEAT">>;
timezone_to_abbreviation(false, <<"Africa/Kampala">>) -> <<"BEAT">>;
timezone_to_abbreviation(false, <<"Africa/Nairobi">>) -> <<"BEAT">>;
timezone_to_abbreviation(false, <<"Africa/Dar_es_Salaam">>) -> <<"BEAUT">>;
timezone_to_abbreviation(false, <<"Europe/Tiraspol">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"America/Bogota">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"Asia/Bangkok">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"Asia/Jakarta">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"Europe/Bucharest">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"Europe/Chisinau">>) -> <<"BMT">>;
timezone_to_abbreviation(false, <<"Asia/Brunei">>) -> <<"BNT">>;
timezone_to_abbreviation(true, <<"Asia/Kuching">>) -> <<"BORTST">>;
timezone_to_abbreviation(false, <<"Asia/Kuching">>) -> <<"BORT">>;
timezone_to_abbreviation(true, <<"America/La_Paz">>) -> <<"BOST">>;
timezone_to_abbreviation(false, <<"America/La_Paz">>) -> <<"BOT">>;
timezone_to_abbreviation(true, <<"America/Sao_Paulo">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Araguaina">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Bahia">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Belem">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Fortaleza">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Maceio">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"America/Recife">>) -> <<"BRST">>;
timezone_to_abbreviation(true, <<"Brazil/East">>) -> <<"BRST">>;
timezone_to_abbreviation(false, <<"America/Sao_Paulo">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Araguaina">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Bahia">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Belem">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Fortaleza">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Maceio">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"America/Recife">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"Brazil/East">>) -> <<"BRT">>;
timezone_to_abbreviation(false, <<"Europe/London">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Pacific/Midway">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Pacific/Pago_Pago">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Pacific/Samoa">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Europe/Belfast">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Europe/Guernsey">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Europe/Isle_of_Man">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Europe/Jersey">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"GB">>) -> <<"BST">>;
timezone_to_abbreviation(true, <<"Europe/Dublin">>) -> <<"BST">>;
timezone_to_abbreviation(false, <<"Asia/Thimbu">>) -> <<"BTT">>;
timezone_to_abbreviation(false, <<"Asia/Thimphu">>) -> <<"BTT">>;
timezone_to_abbreviation(false, <<"Asia/Kolkata">>) -> <<"BURT">>;
timezone_to_abbreviation(false, <<"Asia/Calcutta">>) -> <<"BURT">>;
timezone_to_abbreviation(false, <<"Asia/Rangoon">>) -> <<"BURT">>;
timezone_to_abbreviation(false, <<"Atlantic/Canary">>) -> <<"CANT">>;
timezone_to_abbreviation(true, <<"Africa/Gaborone">>) -> <<"CAST">>;
timezone_to_abbreviation(true, <<"Africa/Juba">>) -> <<"CAST">>;
timezone_to_abbreviation(true, <<"Africa/Khartoum">>) -> <<"CAST">>;
timezone_to_abbreviation(false, <<"Africa/Khartoum">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Blantyre">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Bujumbura">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Gaborone">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Harare">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Juba">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Kigali">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Lubumbashi">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Lusaka">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Maputo">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Africa/Windhoek">>) -> <<"CAT">>;
timezone_to_abbreviation(false, <<"Indian/Cocos">>) -> <<"CCT">>;
timezone_to_abbreviation(true, <<"America/Rankin_Inlet">>) -> <<"CDDT">>;
timezone_to_abbreviation(true, <<"America/Resolute">>) -> <<"CDDT">>;
timezone_to_abbreviation(true, <<"America/Chicago">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Asia/Shanghai">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Havana">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Atikokan">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Bahia_Banderas">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Belize">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Cambridge_Bay">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Cancun">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Chihuahua">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Coral_Harbour">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Costa_Rica">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/El_Salvador">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Fort_Wayne">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Guatemala">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Indianapolis">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Knox">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Marengo">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Petersburg">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Tell_City">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Vevay">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Vincennes">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indiana/Winamac">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Indianapolis">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Iqaluit">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Kentucky/Louisville">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Kentucky/Monticello">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Knox_IN">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Louisville">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Managua">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Matamoros">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Menominee">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Merida">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Mexico_City">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Monterrey">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/North_Dakota/Beulah">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/North_Dakota/Center">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/North_Dakota/New_Salem">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Ojinaga">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Rainy_River">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Tegucigalpa">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"America/Winnipeg">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Canada/Central">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Mexico/General">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Asia/Chongqing">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Asia/Chungking">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Asia/Harbin">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Asia/Taipei">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"PRC">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"ROC">>) -> <<"CDT">>;
timezone_to_abbreviation(true, <<"Europe/Berlin">>) -> <<"CEMT">>;
timezone_to_abbreviation(true, <<"CET">>) -> <<"CEMT">>;
timezone_to_abbreviation(true, <<"Europe/Kaliningrad">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Africa/Algiers">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Africa/Ceuta">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Africa/Tripoli">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Africa/Tunis">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Antarctica/Troll">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Arctic/Longyearbyen">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Atlantic/Jan_Mayen">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Amsterdam">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Andorra">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Athens">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Belgrade">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Bratislava">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Brussels">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Budapest">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Busingen">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Chisinau">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Copenhagen">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Kiev">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Lisbon">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Ljubljana">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Luxembourg">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Madrid">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Malta">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Minsk">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Monaco">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Oslo">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Paris">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Podgorica">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Prague">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Riga">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Rome">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/San_Marino">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Sarajevo">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Simferopol">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Skopje">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Sofia">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Stockholm">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Tallinn">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Tirane">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Tiraspol">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Uzhgorod">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Vaduz">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Vatican">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Vienna">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Vilnius">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Warsaw">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Zagreb">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Zaporozhye">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"Europe/Zurich">>) -> <<"CEST">>;
timezone_to_abbreviation(true, <<"WET">>) -> <<"CEST">>;
timezone_to_abbreviation(false, <<"Europe/Berlin">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Kaliningrad">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Africa/Algiers">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Africa/Casablanca">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Africa/Ceuta">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Africa/Tripoli">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Africa/Tunis">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Arctic/Longyearbyen">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Atlantic/Jan_Mayen">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"CET">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Andorra">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Belgrade">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Bratislava">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Brussels">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Budapest">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Busingen">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Copenhagen">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Gibraltar">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Kiev">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Lisbon">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Ljubljana">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Luxembourg">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Madrid">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Malta">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Minsk">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Monaco">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Oslo">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Paris">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Podgorica">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Prague">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Riga">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Rome">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/San_Marino">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Sarajevo">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Simferopol">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Skopje">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Sofia">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Stockholm">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Tallinn">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Tirane">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Uzhgorod">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Vaduz">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Vatican">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Vienna">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Vilnius">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Warsaw">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Zagreb">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Zaporozhye">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"Europe/Zurich">>) -> <<"CET">>;
timezone_to_abbreviation(false, <<"WET">>) -> <<"CET">>;
timezone_to_abbreviation(true, <<"America/Scoresbysund">>) -> <<"CGST">>;
timezone_to_abbreviation(false, <<"America/Scoresbysund">>) -> <<"CGT">>;
timezone_to_abbreviation(true, <<"Pacific/Chatham">>) -> <<"CHADT">>;
timezone_to_abbreviation(false, <<"Pacific/Chatham">>) -> <<"CHAST">>;
timezone_to_abbreviation(true, <<"Asia/Choibalsan">>) -> <<"CHOST">>;
timezone_to_abbreviation(false, <<"Asia/Choibalsan">>) -> <<"CHOT">>;
timezone_to_abbreviation(false, <<"Pacific/Chuuk">>) -> <<"CHUT">>;
timezone_to_abbreviation(false, <<"Pacific/Truk">>) -> <<"CHUT">>;
timezone_to_abbreviation(false, <<"Pacific/Yap">>) -> <<"CHUT">>;
timezone_to_abbreviation(true, <<"Pacific/Rarotonga">>) -> <<"CKHST">>;
timezone_to_abbreviation(false, <<"Pacific/Rarotonga">>) -> <<"CKT">>;
timezone_to_abbreviation(true, <<"America/Santiago">>) -> <<"CLST">>;
timezone_to_abbreviation(true, <<"Chile/Continental">>) -> <<"CLST">>;
timezone_to_abbreviation(false, <<"America/Santiago">>) -> <<"CLT">>;
timezone_to_abbreviation(false, <<"Chile/Continental">>) -> <<"CLT">>;
timezone_to_abbreviation(false, <<"America/Caracas">>) -> <<"CMT">>;
timezone_to_abbreviation(false, <<"America/Panama">>) -> <<"CMT">>;
timezone_to_abbreviation(true, <<"America/Bogota">>) -> <<"COST">>;
timezone_to_abbreviation(false, <<"America/Chicago">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Havana">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Atikokan">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Bahia_Banderas">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Belize">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Cambridge_Bay">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Cancun">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Chihuahua">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Coral_Harbour">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Costa_Rica">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Detroit">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/El_Salvador">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Fort_Wayne">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Guatemala">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Hermosillo">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Indianapolis">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Knox">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Marengo">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Petersburg">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Tell_City">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Vevay">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Vincennes">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indiana/Winamac">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Indianapolis">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Iqaluit">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Kentucky/Louisville">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Kentucky/Monticello">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Knox_IN">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Louisville">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Managua">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Matamoros">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Mazatlan">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Menominee">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Merida">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Mexico_City">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Monterrey">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/North_Dakota/Beulah">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/North_Dakota/Center">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/North_Dakota/New_Salem">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Ojinaga">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Rainy_River">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Rankin_Inlet">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Regina">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Resolute">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Swift_Current">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Tegucigalpa">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Thunder_Bay">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"America/Winnipeg">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Canada/Central">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Canada/East-Saskatchewan">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Canada/Saskatchewan">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Mexico/BajaSur">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Mexico/General">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Chongqing">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Chungking">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Harbin">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Macao">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Macau">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Shanghai">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"Asia/Taipei">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"PRC">>) -> <<"CST">>;
timezone_to_abbreviation(false, <<"ROC">>) -> <<"CST">>;
timezone_to_abbreviation(true, <<"Atlantic/Cape_Verde">>) -> <<"CVST">>;
timezone_to_abbreviation(false, <<"Atlantic/Cape_Verde">>) -> <<"CVT">>;
timezone_to_abbreviation(false, <<"Indian/Christmas">>) -> <<"CXT">>;
timezone_to_abbreviation(false, <<"Pacific/Guam">>) -> <<"CHST">>;
timezone_to_abbreviation(false, <<"Pacific/Saipan">>) -> <<"CHST">>;
timezone_to_abbreviation(false, <<"Antarctica/Davis">>) -> <<"DAVT">>;
timezone_to_abbreviation(false, <<"Antarctica/DumontDUrville">>) -> <<"DDUT">>;
timezone_to_abbreviation(false, <<"Europe/Dublin">>) -> <<"DMT">>;
timezone_to_abbreviation(true, <<"Asia/Dushanbe">>) -> <<"DUSST">>;
timezone_to_abbreviation(false, <<"Asia/Dushanbe">>) -> <<"DUST">>;
timezone_to_abbreviation(true, <<"Chile/EasterIsland">>) -> <<"EASST">>;
timezone_to_abbreviation(true, <<"Pacific/Easter">>) -> <<"EASST">>;
timezone_to_abbreviation(false, <<"Chile/EasterIsland">>) -> <<"EAST">>;
timezone_to_abbreviation(true, <<"Indian/Antananarivo">>) -> <<"EAST">>;
timezone_to_abbreviation(false, <<"Pacific/Easter">>) -> <<"EAST">>;
timezone_to_abbreviation(false, <<"Africa/Djibouti">>) -> <<"EAT">>;
timezone_to_abbreviation(false, <<"Indian/Antananarivo">>) -> <<"EAT">>;
timezone_to_abbreviation(false, <<"Indian/Comoro">>) -> <<"EAT">>;
timezone_to_abbreviation(false, <<"Indian/Mayotte">>) -> <<"EAT">>;
timezone_to_abbreviation(false, <<"America/Guayaquil">>) -> <<"ECT">>;
timezone_to_abbreviation(false, <<"Pacific/Galapagos">>) -> <<"ECT">>;
timezone_to_abbreviation(true, <<"America/New_York">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Detroit">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Grand_Turk">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Jamaica">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Montreal">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Nassau">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Nipigon">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Port-au-Prince">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Santo_Domingo">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Thunder_Bay">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"America/Toronto">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"Canada/Eastern">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"EST">>) -> <<"EDT">>;
timezone_to_abbreviation(true, <<"Europe/Helsinki">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Africa/Cairo">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Amman">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Beirut">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Damascus">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Gaza">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Hebron">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Istanbul">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Asia/Nicosia">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"EET">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Bucharest">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Istanbul">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Mariehamn">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Moscow">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Nicosia">>) -> <<"EEST">>;
timezone_to_abbreviation(true, <<"Europe/Samara">>) -> <<"EEST">>;
timezone_to_abbreviation(false, <<"Europe/Helsinki">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Africa/Cairo">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Amman">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Beirut">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Damascus">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Gaza">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Hebron">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Istanbul">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Asia/Nicosia">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"EET">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Europe/Istanbul">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Europe/Mariehamn">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Europe/Moscow">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"Europe/Nicosia">>) -> <<"EET">>;
timezone_to_abbreviation(false, <<"America/New_York">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Cayman">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Grand_Turk">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Jamaica">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Montreal">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Nassau">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Nipigon">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Port-au-Prince">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"America/Toronto">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"Canada/Eastern">>) -> <<"EST">>;
timezone_to_abbreviation(false, <<"EST">>) -> <<"EST">>;
timezone_to_abbreviation(true, <<"Pacific/Fiji">>) -> <<"FJST">>;
timezone_to_abbreviation(false, <<"Pacific/Fiji">>) -> <<"FJT">>;
timezone_to_abbreviation(true, <<"Atlantic/Stanley">>) -> <<"FKST">>;
timezone_to_abbreviation(false, <<"Atlantic/Stanley">>) -> <<"FKST">>;
timezone_to_abbreviation(false, <<"Atlantic/Madeira">>) -> <<"FMT">>;
timezone_to_abbreviation(true, <<"America/Noronha">>) -> <<"FNST">>;
timezone_to_abbreviation(true, <<"Brazil/DeNoronha">>) -> <<"FNST">>;
timezone_to_abbreviation(false, <<"America/Noronha">>) -> <<"FNT">>;
timezone_to_abbreviation(false, <<"Brazil/DeNoronha">>) -> <<"FNT">>;
timezone_to_abbreviation(true, <<"Asia/Bishkek">>) -> <<"FRUST">>;
timezone_to_abbreviation(false, <<"Asia/Bishkek">>) -> <<"FRUT">>;
timezone_to_abbreviation(false, <<"Pacific/Gambier">>) -> <<"GAMT">>;
timezone_to_abbreviation(false, <<"America/Guyana">>) -> <<"GBGT">>;
timezone_to_abbreviation(true, <<"Asia/Tbilisi">>) -> <<"GEST">>;
timezone_to_abbreviation(false, <<"Asia/Tbilisi">>) -> <<"GET">>;
timezone_to_abbreviation(false, <<"America/Cayenne">>) -> <<"GFT">>;
timezone_to_abbreviation(true, <<"Africa/Accra">>) -> <<"GHST">>;
timezone_to_abbreviation(false, <<"Pacific/Tarawa">>) -> <<"GILT">>;
timezone_to_abbreviation(false, <<"Africa/Abidjan">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Accra">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Bamako">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Banjul">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Bissau">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Conakry">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Dakar">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Freetown">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Lome">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Malabo">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Monrovia">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Niamey">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Nouakchott">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Ouagadougou">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Porto-Novo">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Sao_Tome">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Africa/Timbuktu">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"America/Danmarkshavn">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Atlantic/Reykjavik">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Atlantic/St_Helena">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Etc/GMT">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Etc/Greenwich">>) -> <<"GMT">>;
timezone_to_abbreviation(false, <<"Asia/Dubai">>) -> <<"GST">>;
timezone_to_abbreviation(false, <<"Atlantic/South_Georgia">>) -> <<"GST">>;
timezone_to_abbreviation(false, <<"Asia/Muscat">>) -> <<"GST">>;
timezone_to_abbreviation(true, <<"Pacific/Honolulu">>) -> <<"HDT">>;
timezone_to_abbreviation(true, <<"HST">>) -> <<"HDT">>;
timezone_to_abbreviation(true, <<"Pacific/Johnston">>) -> <<"HDT">>;
timezone_to_abbreviation(true, <<"Asia/Hong_Kong">>) -> <<"HKST">>;
timezone_to_abbreviation(false, <<"Asia/Hong_Kong">>) -> <<"HKT">>;
timezone_to_abbreviation(true, <<"Asia/Hovd">>) -> <<"HOVST">>;
timezone_to_abbreviation(false, <<"Asia/Hovd">>) -> <<"HOVT">>;
timezone_to_abbreviation(false, <<"Pacific/Honolulu">>) -> <<"HST">>;
timezone_to_abbreviation(false, <<"HST">>) -> <<"HST">>;
timezone_to_abbreviation(false, <<"Pacific/Johnston">>) -> <<"HST">>;
timezone_to_abbreviation(false, <<"Asia/Ho_Chi_Minh">>) -> <<"ICT">>;
timezone_to_abbreviation(false, <<"Asia/Phnom_Penh">>) -> <<"ICT">>;
timezone_to_abbreviation(false, <<"Asia/Saigon">>) -> <<"ICT">>;
timezone_to_abbreviation(false, <<"Asia/Vientiane">>) -> <<"ICT">>;
timezone_to_abbreviation(true, <<"Asia/Jerusalem">>) -> <<"IDDT">>;
timezone_to_abbreviation(true, <<"Asia/Tel_Aviv">>) -> <<"IDDT">>;
timezone_to_abbreviation(true, <<"Asia/Colombo">>) -> <<"IHST">>;
timezone_to_abbreviation(false, <<"Asia/Irkutsk">>) -> <<"IMT">>;
timezone_to_abbreviation(false, <<"Indian/Chagos">>) -> <<"IOT">>;
timezone_to_abbreviation(true, <<"Asia/Tehran">>) -> <<"IRDT">>;
timezone_to_abbreviation(true, <<"Asia/Irkutsk">>) -> <<"IRKST">>;
timezone_to_abbreviation(false, <<"Asia/Chita">>) -> <<"IRKT">>;
timezone_to_abbreviation(false, <<"Asia/Tehran">>) -> <<"IRST">>;
timezone_to_abbreviation(true, <<"Atlantic/Reykjavik">>) -> <<"ISST">>;
timezone_to_abbreviation(false, <<"Asia/Jerusalem">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Colombo">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Karachi">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Kathmandu">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Katmandu">>) -> <<"IST">>;
timezone_to_abbreviation(true, <<"Asia/Calcutta">>) -> <<"IST">>;
timezone_to_abbreviation(true, <<"Asia/Karachi">>) -> <<"IST">>;
timezone_to_abbreviation(true, <<"Asia/Kolkata">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Tel_Aviv">>) -> <<"IST">>;
timezone_to_abbreviation(false, <<"Asia/Pyongyang">>) -> <<"JCST">>;
timezone_to_abbreviation(false, <<"Asia/Sakhalin">>) -> <<"JCST">>;
timezone_to_abbreviation(false, <<"Asia/Seoul">>) -> <<"JCST">>;
timezone_to_abbreviation(false, <<"Asia/Tokyo">>) -> <<"JCST">>;
timezone_to_abbreviation(false, <<"ROK">>) -> <<"JCST">>;
timezone_to_abbreviation(true, <<"Asia/Tokyo">>) -> <<"JDT">>;
timezone_to_abbreviation(false, <<"Asia/Dili">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Kuala_Lumpur">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Makassar">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Manila">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Pontianak">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Singapore">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Asia/Ujung_Pandang">>) -> <<"JST">>;
timezone_to_abbreviation(false, <<"Pacific/Nauru">>) -> <<"JST">>;
timezone_to_abbreviation(true, <<"Asia/Seoul">>) -> <<"KDT">>;
timezone_to_abbreviation(true, <<"ROK">>) -> <<"KDT">>;
timezone_to_abbreviation(true, <<"Asia/Qyzylorda">>) -> <<"KIZST">>;
timezone_to_abbreviation(false, <<"Asia/Qyzylorda">>) -> <<"KIZT">>;
timezone_to_abbreviation(false, <<"Pacific/Kosrae">>) -> <<"KOST">>;
timezone_to_abbreviation(true, <<"Asia/Krasnoyarsk">>) -> <<"KRAST">>;
timezone_to_abbreviation(true, <<"Asia/Novokuznetsk">>) -> <<"KRAST">>;
timezone_to_abbreviation(false, <<"Asia/Krasnoyarsk">>) -> <<"KRAT">>;
timezone_to_abbreviation(false, <<"Asia/Novokuznetsk">>) -> <<"KRAT">>;
timezone_to_abbreviation(false, <<"Europe/Samara">>) -> <<"KUYT">>;
timezone_to_abbreviation(false, <<"Pacific/Kwajalein">>) -> <<"KWAT">>;
timezone_to_abbreviation(true, <<"Australia/LHI">>) -> <<"LHDT">>;
timezone_to_abbreviation(true, <<"Australia/Lord_Howe">>) -> <<"LHDT">>;
timezone_to_abbreviation(false, <<"Pacific/Kiritimati">>) -> <<"LINT">>;
timezone_to_abbreviation(true, <<"Atlantic/Madeira">>) -> <<"MADMT">>;
timezone_to_abbreviation(true, <<"Asia/Magadan">>) -> <<"MAGST">>;
timezone_to_abbreviation(true, <<"Asia/Srednekolymsk">>) -> <<"MAGST">>;
timezone_to_abbreviation(true, <<"Asia/Ust-Nera">>) -> <<"MAGST">>;
timezone_to_abbreviation(false, <<"Asia/Magadan">>) -> <<"MAGT">>;
timezone_to_abbreviation(false, <<"Asia/Srednekolymsk">>) -> <<"MAGT">>;
timezone_to_abbreviation(false, <<"Asia/Ust-Nera">>) -> <<"MAGT">>;
timezone_to_abbreviation(true, <<"Asia/Singapore">>) -> <<"MALST">>;
timezone_to_abbreviation(true, <<"Asia/Kuala_Lumpur">>) -> <<"MALST">>;
timezone_to_abbreviation(false, <<"Pacific/Marquesas">>) -> <<"MART">>;
timezone_to_abbreviation(false, <<"Antarctica/Mawson">>) -> <<"MAWT">>;
timezone_to_abbreviation(true, <<"America/Yellowknife">>) -> <<"MDDT">>;
timezone_to_abbreviation(true, <<"America/Denver">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Boise">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Edmonton">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Hermosillo">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Inuvik">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Mazatlan">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Phoenix">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Regina">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Shiprock">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"America/Swift_Current">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"Canada/East-Saskatchewan">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"Canada/Mountain">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"Canada/Saskatchewan">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"Mexico/BajaSur">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"MST">>) -> <<"MDT">>;
timezone_to_abbreviation(true, <<"MET">>) -> <<"MEST">>;
timezone_to_abbreviation(false, <<"MET">>) -> <<"MET">>;
timezone_to_abbreviation(false, <<"Pacific/Majuro">>) -> <<"MHT">>;
timezone_to_abbreviation(false, <<"America/Montevideo">>) -> <<"MMT">>;
timezone_to_abbreviation(false, <<"Indian/Maldives">>) -> <<"MMT">>;
timezone_to_abbreviation(true, <<"Asia/Macao">>) -> <<"MOST">>;
timezone_to_abbreviation(true, <<"Asia/Macau">>) -> <<"MOST">>;
timezone_to_abbreviation(false, <<"Europe/Volgograd">>) -> <<"MSK">>;
timezone_to_abbreviation(true, <<"Europe/Volgograd">>) -> <<"MSK">>;
timezone_to_abbreviation(false, <<"America/Denver">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Boise">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Creston">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Dawson_Creek">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Edmonton">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Ensenada">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Inuvik">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Phoenix">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Santa_Isabel">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Shiprock">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Tijuana">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"America/Yellowknife">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"Canada/Mountain">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"Mexico/BajaNorte">>) -> <<"MST">>;
timezone_to_abbreviation(false, <<"MST">>) -> <<"MST">>;
timezone_to_abbreviation(true, <<"Indian/Mauritius">>) -> <<"MUST">>;
timezone_to_abbreviation(false, <<"Indian/Mauritius">>) -> <<"MUT">>;
timezone_to_abbreviation(true, <<"Pacific/Noumea">>) -> <<"NCST">>;
timezone_to_abbreviation(false, <<"Pacific/Noumea">>) -> <<"NCT">>;
timezone_to_abbreviation(true, <<"America/St_Johns">>) -> <<"NDDT">>;
timezone_to_abbreviation(true, <<"Canada/Newfoundland">>) -> <<"NDDT">>;
timezone_to_abbreviation(true, <<"Pacific/Midway">>) -> <<"NDT">>;
timezone_to_abbreviation(false, <<"America/Paramaribo">>) -> <<"NEGT">>;
timezone_to_abbreviation(false, <<"Pacific/Norfolk">>) -> <<"NFT">>;
timezone_to_abbreviation(true, <<"Asia/Novosibirsk">>) -> <<"NOVST">>;
timezone_to_abbreviation(false, <<"Asia/Novosibirsk">>) -> <<"NOVT">>;
timezone_to_abbreviation(false, <<"America/St_Johns">>) -> <<"NST">>;
timezone_to_abbreviation(false, <<"Canada/Newfoundland">>) -> <<"NST">>;
timezone_to_abbreviation(false, <<"Pacific/Niue">>) -> <<"NUT">>;
timezone_to_abbreviation(true, <<"Pacific/Auckland">>) -> <<"NZDT">>;
timezone_to_abbreviation(true, <<"Antarctica/McMurdo">>) -> <<"NZDT">>;
timezone_to_abbreviation(true, <<"Antarctica/South_Pole">>) -> <<"NZDT">>;
timezone_to_abbreviation(true, <<"NZ">>) -> <<"NZDT">>;
timezone_to_abbreviation(false, <<"Pacific/Auckland">>) -> <<"NZMT">>;
timezone_to_abbreviation(false, <<"Antarctica/McMurdo">>) -> <<"NZMT">>;
timezone_to_abbreviation(false, <<"Antarctica/South_Pole">>) -> <<"NZMT">>;
timezone_to_abbreviation(false, <<"NZ">>) -> <<"NZMT">>;
timezone_to_abbreviation(true, <<"Asia/Omsk">>) -> <<"OMSST">>;
timezone_to_abbreviation(false, <<"Asia/Omsk">>) -> <<"OMST">>;
timezone_to_abbreviation(true, <<"Asia/Oral">>) -> <<"ORAST">>;
timezone_to_abbreviation(false, <<"Asia/Oral">>) -> <<"ORAT">>;
timezone_to_abbreviation(true, <<"America/Los_Angeles">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Dawson">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Dawson_Creek">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Ensenada">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Metlakatla">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Santa_Isabel">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Tijuana">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Vancouver">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Whitehorse">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"Canada/Pacific">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"Canada/Yukon">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"Mexico/BajaNorte">>) -> <<"PDT">>;
timezone_to_abbreviation(true, <<"America/Lima">>) -> <<"PEST">>;
timezone_to_abbreviation(true, <<"Asia/Kamchatka">>) -> <<"PETST">>;
timezone_to_abbreviation(false, <<"Asia/Kamchatka">>) -> <<"PETT">>;
timezone_to_abbreviation(false, <<"America/Lima">>) -> <<"PET">>;
timezone_to_abbreviation(false, <<"Pacific/Port_Moresby">>) -> <<"PGT">>;
timezone_to_abbreviation(false, <<"Pacific/Enderbury">>) -> <<"PHOT">>;
timezone_to_abbreviation(true, <<"Asia/Manila">>) -> <<"PHST">>;
timezone_to_abbreviation(true, <<"America/Miquelon">>) -> <<"PMDT">>;
timezone_to_abbreviation(false, <<"Pacific/Pitcairn">>) -> <<"PNT">>;
timezone_to_abbreviation(false, <<"Pacific/Pohnpei">>) -> <<"PONT">>;
timezone_to_abbreviation(false, <<"Pacific/Ponape">>) -> <<"PONT">>;
timezone_to_abbreviation(false, <<"America/Los_Angeles">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"America/Dawson">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"America/Metlakatla">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"America/Vancouver">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"America/Whitehorse">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"Canada/Pacific">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"Canada/Yukon">>) -> <<"PST">>;
timezone_to_abbreviation(false, <<"Pacific/Palau">>) -> <<"PWT">>;
timezone_to_abbreviation(true, <<"America/Asuncion">>) -> <<"PYST">>;
timezone_to_abbreviation(false, <<"Indian/Reunion">>) -> <<"RET">>;
timezone_to_abbreviation(false, <<"Antarctica/Rothera">>) -> <<"ROTT">>;
timezone_to_abbreviation(true, <<"Asia/Sakhalin">>) -> <<"SAKST">>;
timezone_to_abbreviation(true, <<"Asia/Samarkand">>) -> <<"SAMST">>;
timezone_to_abbreviation(false, <<"Asia/Samarkand">>) -> <<"SAMT">>;
timezone_to_abbreviation(false, <<"Africa/Johannesburg">>) -> <<"SAST">>;
timezone_to_abbreviation(true, <<"Africa/Johannesburg">>) -> <<"SAST">>;
timezone_to_abbreviation(true, <<"Africa/Maseru">>) -> <<"SAST">>;
timezone_to_abbreviation(true, <<"Africa/Windhoek">>) -> <<"SAST">>;
timezone_to_abbreviation(false, <<"Africa/Maseru">>) -> <<"SAST">>;
timezone_to_abbreviation(false, <<"Africa/Mbabane">>) -> <<"SAST">>;
timezone_to_abbreviation(false, <<"Pacific/Guadalcanal">>) -> <<"SBT">>;
timezone_to_abbreviation(false, <<"Indian/Mahe">>) -> <<"SCT">>;
timezone_to_abbreviation(true, <<"Pacific/Apia">>) -> <<"SDT">>;
timezone_to_abbreviation(false, <<"Pacific/Apia">>) -> <<"SST">>;
timezone_to_abbreviation(true, <<"Asia/Yekaterinburg">>) -> <<"SVEST">>;
timezone_to_abbreviation(false, <<"Asia/Yekaterinburg">>) -> <<"SVET">>;
timezone_to_abbreviation(false, <<"Antarctica/Syowa">>) -> <<"SYOT">>;
timezone_to_abbreviation(false, <<"Pacific/Tahiti">>) -> <<"TAHT">>;
timezone_to_abbreviation(true, <<"Asia/Tashkent">>) -> <<"TASST">>;
timezone_to_abbreviation(false, <<"Asia/Tashkent">>) -> <<"TAST">>;
timezone_to_abbreviation(false, <<"Indian/Kerguelen">>) -> <<"TFT">>;
timezone_to_abbreviation(false, <<"Pacific/Fakaofo">>) -> <<"TKT">>;
timezone_to_abbreviation(true, <<"Pacific/Tongatapu">>) -> <<"TOST">>;
timezone_to_abbreviation(false, <<"Pacific/Tongatapu">>) -> <<"TOT">>;
timezone_to_abbreviation(false, <<"Pacific/Funafuti">>) -> <<"TVT">>;
timezone_to_abbreviation(false, <<"Etc/UCT">>) -> <<"UCT">>;
timezone_to_abbreviation(false, <<"UCT">>) -> <<"UCT">>;
timezone_to_abbreviation(true, <<"Asia/Ulaanbaatar">>) -> <<"ULAST">>;
timezone_to_abbreviation(true, <<"Asia/Ulan_Bator">>) -> <<"ULAST">>;
timezone_to_abbreviation(false, <<"Asia/Ulaanbaatar">>) -> <<"ULAT">>;
timezone_to_abbreviation(false, <<"Asia/Ulan_Bator">>) -> <<"ULAT">>;
timezone_to_abbreviation(false, <<"Antarctica/Troll">>) -> <<"UTC">>;
timezone_to_abbreviation(false, <<"Etc/Universal">>) -> <<"UTC">>;
timezone_to_abbreviation(false, <<"Etc/UTC">>) -> <<"UTC">>;
timezone_to_abbreviation(false, <<"Etc/Zulu">>) -> <<"UTC">>;
timezone_to_abbreviation(false, <<"GMT">>) -> <<"UTC">>;
timezone_to_abbreviation(false, <<"UTC">>) -> <<"UTC">>;
timezone_to_abbreviation(true, <<"America/Montevideo">>) -> <<"UYHST">>;
timezone_to_abbreviation(true, <<"Asia/Vladivostok">>) -> <<"VLAST">>;
timezone_to_abbreviation(true, <<"Asia/Khandyga">>) -> <<"VLAST">>;
timezone_to_abbreviation(false, <<"Asia/Vladivostok">>) -> <<"VLAT">>;
timezone_to_abbreviation(false, <<"Asia/Khandyga">>) -> <<"VLAT">>;
timezone_to_abbreviation(false, <<"Antarctica/Vostok">>) -> <<"VOST">>;
timezone_to_abbreviation(true, <<"Pacific/Efate">>) -> <<"VUST">>;
timezone_to_abbreviation(false, <<"Pacific/Efate">>) -> <<"VUT">>;
timezone_to_abbreviation(false, <<"Pacific/Wake">>) -> <<"WAKT">>;
timezone_to_abbreviation(true, <<"Africa/Ndjamena">>) -> <<"WAST">>;
timezone_to_abbreviation(false, <<"Africa/Brazzaville">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/El_Aaiun">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Bangui">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Douala">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Kinshasa">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Lagos">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Libreville">>) -> <<"WAT">>;
timezone_to_abbreviation(false, <<"Africa/Ndjamena">>) -> <<"WAT">>;
timezone_to_abbreviation(true, <<"Africa/Casablanca">>) -> <<"WEST">>;
timezone_to_abbreviation(true, <<"Africa/El_Aaiun">>) -> <<"WEST">>;
timezone_to_abbreviation(true, <<"Atlantic/Canary">>) -> <<"WEST">>;
timezone_to_abbreviation(true, <<"Atlantic/Faeroe">>) -> <<"WEST">>;
timezone_to_abbreviation(true, <<"Atlantic/Faroe">>) -> <<"WEST">>;
timezone_to_abbreviation(false, <<"Atlantic/Faeroe">>) -> <<"WET">>;
timezone_to_abbreviation(false, <<"Atlantic/Faroe">>) -> <<"WET">>;
timezone_to_abbreviation(false, <<"Pacific/Wallis">>) -> <<"WFT">>;
timezone_to_abbreviation(true, <<"America/Godthab">>) -> <<"WGST">>;
timezone_to_abbreviation(true, <<"America/Danmarkshavn">>) -> <<"WGST">>;
timezone_to_abbreviation(false, <<"America/Godthab">>) -> <<"WGT">>;
timezone_to_abbreviation(false, <<"Asia/Kashgar">>) -> <<"XJT">>;
timezone_to_abbreviation(false, <<"Asia/Urumqi">>) -> <<"XJT">>;
timezone_to_abbreviation(true, <<"Asia/Yakutsk">>) -> <<"YAKST">>;
timezone_to_abbreviation(true, <<"Asia/Chita">>) -> <<"YAKST">>;
timezone_to_abbreviation(false, <<"Asia/Yakutsk">>) -> <<"YAKT">>;
timezone_to_abbreviation(_, Timezone) -> Timezone.

-spec abbreviations() -> [abbreviation()].

abbreviations() -> [
    <<"ACDT">>,<<"ACST">>,<<"ACT">>,<<"ACWDT">>,<<"ACWST">>,
    <<"ADDT">>,<<"ADMT">>,<<"ADT">>,<<"AEDT">>,<<"AEST">>,
    <<"AFT">>,<<"AHDT">>,<<"AHST">>,<<"AKDT">>,<<"AKST">>,
    <<"AKTST">>,<<"AKTT">>,<<"ALMST">>,<<"ALMT">>,<<"AMST">>,
    <<"AMT">>,<<"ANAST">>,<<"ANAT">>,<<"ANT">>,<<"AOT">>,
    <<"APT">>,<<"AQTST">>,<<"AQTT">>,<<"ARST">>,<<"ART">>,
    <<"ASHST">>,<<"ASHT">>,<<"AST">>,<<"AWDT">>,<<"AWST">>,
    <<"AWT">>,<<"AZOMT">>,<<"AZOST">>,<<"AZOT">>,<<"AZST">>,
    <<"AZT">>,<<"BAKST">>,<<"BAKT">>,<<"BDST">>,<<"BDT">>,
    <<"BEAT">>,<<"BEAUT">>,<<"BMT">>,<<"BNT">>,<<"BORTST">>,
    <<"BORT">>,<<"BOST">>,<<"BOT">>,<<"BRST">>,<<"BRT">>,
    <<"BST">>,<<"BTT">>,<<"BURT">>,<<"CANT">>,<<"CAPT">>,
    <<"CAST">>,<<"CAT">>,<<"CAWT">>,<<"CCT">>,<<"CDDT">>,
    <<"CDT">>,<<"CEMT">>,<<"CEST">>,<<"CET">>,<<"CGST">>,
    <<"CGT">>,<<"CHADT">>,<<"CHAST">>,<<"CHDT">>,<<"CHOST">>,
    <<"CHOT">>,<<"CHUT">>,<<"CKHST">>,<<"CKT">>,<<"CLST">>,
    <<"CLT">>,<<"CMT">>,<<"COST">>,<<"COT">>,<<"CPT">>,
    <<"CST">>,<<"CUT">>,<<"CVST">>,<<"CVT">>,<<"CWT">>,
    <<"CXT">>,<<"CHST">>,<<"DACT">>,<<"DAVT">>,<<"DDUT">>,
    <<"DMT">>,<<"DUSST">>,<<"DUST">>,<<"EASST">>,<<"EAST">>,
    <<"EAT">>,<<"ECT">>,<<"EDDT">>,<<"EDT">>,<<"EEST">>,
    <<"EET">>,<<"EGST">>,<<"EGT">>,<<"EHDT">>,<<"EMT">>,
    <<"EPT">>,<<"EST">>,<<"EWT">>,<<"FET">>,<<"FFMT">>,
    <<"FJST">>,<<"FJT">>,<<"FKST">>,<<"FKT">>,<<"FMT">>,
    <<"FNST">>,<<"FNT">>,<<"FORT">>,<<"FRUST">>,<<"FRUT">>,
    <<"GALT">>,<<"GAMT">>,<<"GBGT">>,<<"GEST">>,<<"GET">>,
    <<"GFT">>,<<"GHST">>,<<"GILT">>,<<"GMT">>,<<"GST">>,
    <<"GYT">>,<<"HADT">>,<<"HAST">>,<<"HDT">>,<<"HKST">>,
    <<"HKT">>,<<"HMT">>,<<"HOVST">>,<<"HOVT">>,<<"HST">>,
    <<"ICT">>,<<"IDDT">>,<<"IDT">>,<<"IHST">>,<<"IMT">>,
    <<"IOT">>,<<"IRDT">>,<<"IRKST">>,<<"IRKT">>,<<"IRST">>,
    <<"ISST">>,<<"IST">>,<<"JAVT">>,<<"JCST">>,<<"JDT">>,
    <<"JMT">>,<<"JST">>,<<"JWST">>,<<"KART">>,<<"KDT">>,
    <<"KGST">>,<<"KGT">>,<<"KIZST">>,<<"KIZT">>,<<"KMT">>,
    <<"KOST">>,<<"KRAST">>,<<"KRAT">>,<<"KST">>,<<"KUYST">>,
    <<"KUYT">>,<<"KWAT">>,<<"LHDT">>,<<"LHST">>,<<"LINT">>,
    <<"LKT">>,<<"LRT">>,<<"LST">>,<<"MADMT">>,<<"MADST">>,
    <<"MADT">>,<<"MAGST">>,<<"MAGT">>,<<"MALST">>,<<"MALT">>,
    <<"MART">>,<<"MAWT">>,<<"MDDT">>,<<"MDST">>,<<"MDT">>,
    <<"MEST">>,<<"MET">>,<<"MHT">>,<<"MIST">>,<<"MMT">>,
    <<"MOST">>,<<"MOT">>,<<"MPT">>,<<"MSD">>,<<"MSK">>,
    <<"MSM">>,<<"MST">>,<<"MUST">>,<<"MUT">>,<<"MVT">>,
    <<"MWT">>,<<"MYT">>,<<"NCST">>,<<"NCT">>,<<"NDDT">>,
    <<"NDT">>,<<"NEGT">>,<<"NEST">>,<<"NET">>,<<"NFT">>,
    <<"NMT">>,<<"NOVST">>,<<"NOVT">>,<<"NPT">>,<<"NRT">>,
    <<"NST">>,<<"NUT">>,<<"NWT">>,<<"NZDT">>,<<"NZMT">>,
    <<"NZST">>,<<"OMSST">>,<<"OMST">>,<<"ORAST">>,<<"ORAT">>,
    <<"PDDT">>,<<"PDT">>,<<"PEST">>,<<"PETST">>,<<"PETT">>,
    <<"PET">>,<<"PGT">>,<<"PHOT">>,<<"PHST">>,<<"PHT">>,
    <<"PKST">>,<<"PKT">>,<<"PMDT">>,<<"PMST">>,<<"PMT">>,
    <<"PNT">>,<<"PONT">>,<<"PPMT">>,<<"PPT">>,<<"PST">>,
    <<"PWT">>,<<"PYST">>,<<"PYT">>,<<"QMT">>,<<"QYZST">>,
    <<"QYZT">>,<<"RET">>,<<"RMT">>,<<"ROTT">>,<<"SAKST">>,
    <<"SAKT">>,<<"SAMST">>,<<"SAMT">>,<<"SAST">>,<<"SBT">>,
    <<"SCT">>,<<"SDMT">>,<<"SDT">>,<<"SGT">>,<<"SHEST">>,
    <<"SHET">>,<<"SJMT">>,<<"SMT">>,<<"SRET">>,<<"SRT">>,
    <<"SST">>,<<"STAT">>,<<"SVEST">>,<<"SVET">>,<<"SWAT">>,
    <<"SYOT">>,<<"TAHT">>,<<"TASST">>,<<"TAST">>,<<"TBIST">>,
    <<"TBIT">>,<<"TBMT">>,<<"TFT">>,<<"TJT">>,<<"TKT">>,
    <<"TLT">>,<<"TMT">>,<<"TOST">>,<<"TOT">>,<<"TRST">>,
    <<"TRT">>,<<"TSAT">>,<<"TVT">>,<<"UCT">>,<<"ULAST">>,
    <<"ULAT">>,<<"URAST">>,<<"URAT">>,<<"UTC">>,<<"UYHST">>,
    <<"UYST">>,<<"UYT">>,<<"UZST">>,<<"UZT">>,<<"VET">>,
    <<"VLAST">>,<<"VLAT">>,<<"VOLST">>,<<"VOLT">>,<<"VOST">>,
    <<"VUST">>,<<"VUT">>,<<"WAKT">>,<<"WARST">>,<<"WART">>,
    <<"WAST">>,<<"WAT">>,<<"WEMT">>,<<"WEST">>,<<"WET">>,
    <<"WFT">>,<<"WGST">>,<<"WGT">>,<<"WIB">>,<<"WITA">>,
    <<"WIT">>,<<"WMT">>,<<"WSDT">>,<<"WSST">>,<<"XJT">>,
    <<"YAKST">>,<<"YAKT">>,<<"YDDT">>,<<"YDT">>,<<"YEKST">>,
    <<"YEKT">>,<<"YERST">>,<<"YERT">>,<<"YPT">>,<<"YST">>,
    <<"YWT">>,<<"A">>,<<"B">>,<<"C">>,<<"D">>,
    <<"E">>,<<"F">>,<<"G">>,<<"H">>,<<"I">>,
    <<"K">>,<<"L">>,<<"M">>,<<"N">>,<<"O">>,
    <<"P">>,<<"Q">>,<<"R">>,<<"S">>,<<"T">>,
    <<"U">>,<<"V">>,<<"W">>,<<"X">>,<<"Y">>,
    <<"ZZZ">>,<<"Z">>
].

-spec timezone_info(abbreviation()) -> [timezone_info()].

timezone_info(<<"ACDT">>) -> [
    {true, 37800, <<"Australia/Adelaide">>},
    {true, 37800, <<"Australia/Broken_Hill">>},
    {true, 37800, <<"Australia/Darwin">>},
    {true, 37800, <<"Australia/North">>},
    {true, 37800, <<"Australia/South">>},
    {true, 37800, <<"Australia/Yancowinna">>}
];
timezone_info(<<"ACST">>) -> [
    {true, -14400, <<"America/Porto_Acre">>},
    {false, 34200, <<"Australia/Adelaide">>},
    {true, -14400, <<"America/Eirunepe">>},
    {true, -14400, <<"America/Rio_Branco">>},
    {true, -14400, <<"Brazil/Acre">>},
    {false, 34200, <<"Asia/Jayapura">>},
    {false, 34200, <<"Australia/Broken_Hill">>},
    {false, 34200, <<"Australia/Darwin">>},
    {false, 34200, <<"Australia/North">>},
    {false, 34200, <<"Australia/South">>},
    {false, 34200, <<"Australia/Yancowinna">>}
];
timezone_info(<<"ACT">>) -> [
    {false, -18000, <<"America/Porto_Acre">>},
    {false, -18000, <<"America/Eirunepe">>},
    {false, -18000, <<"America/Rio_Branco">>},
    {false, -18000, <<"Brazil/Acre">>}
];
timezone_info(<<"ACWDT">>) -> [
    {true, 35100, <<"Australia/Eucla">>}
];
timezone_info(<<"ACWST">>) -> [
    {false, 31500, <<"Australia/Eucla">>}
];
timezone_info(<<"ADDT">>) -> [
    {true, -7200, <<"America/Goose_Bay">>},
    {true, -7200, <<"America/Pangnirtung">>}
];
timezone_info(<<"ADMT">>) -> [
    {false, 9320, <<"Africa/Addis_Ababa">>},
    {false, 9320, <<"Africa/Asmara">>},
    {false, 9320, <<"Africa/Asmera">>}
];
timezone_info(<<"ADT">>) -> [
    {true, -10800, <<"America/Halifax">>},
    {true, -10800, <<"America/Barbados">>},
    {true, -10800, <<"America/Blanc-Sablon">>},
    {true, -10800, <<"America/Glace_Bay">>},
    {true, -10800, <<"America/Goose_Bay">>},
    {true, -10800, <<"America/Martinique">>},
    {true, -10800, <<"America/Moncton">>},
    {true, -10800, <<"America/Pangnirtung">>},
    {true, -10800, <<"America/Thule">>},
    {true, -10800, <<"Atlantic/Bermuda">>},
    {true, -10800, <<"Canada/Atlantic">>},
    {true, 14400, <<"Asia/Baghdad">>}
];
timezone_info(<<"AEDT">>) -> [
    {true, 39600, <<"Australia/Melbourne">>},
    {true, 39600, <<"Antarctica/Macquarie">>},
    {true, 39600, <<"Australia/ACT">>},
    {true, 39600, <<"Australia/Brisbane">>},
    {true, 39600, <<"Australia/Canberra">>},
    {true, 39600, <<"Australia/Currie">>},
    {true, 39600, <<"Australia/Hobart">>},
    {true, 39600, <<"Australia/Lindeman">>},
    {true, 39600, <<"Australia/NSW">>},
    {true, 39600, <<"Australia/Queensland">>},
    {true, 39600, <<"Australia/Sydney">>},
    {true, 39600, <<"Australia/Tasmania">>},
    {true, 39600, <<"Australia/Victoria">>}
];
timezone_info(<<"AEST">>) -> [
    {false, 36000, <<"Australia/Melbourne">>},
    {false, 36000, <<"Antarctica/Macquarie">>},
    {false, 36000, <<"Australia/ACT">>},
    {false, 36000, <<"Australia/Brisbane">>},
    {false, 36000, <<"Australia/Canberra">>},
    {false, 36000, <<"Australia/Currie">>},
    {false, 36000, <<"Australia/Hobart">>},
    {false, 36000, <<"Australia/LHI">>},
    {false, 36000, <<"Australia/Lindeman">>},
    {false, 36000, <<"Australia/Lord_Howe">>},
    {false, 36000, <<"Australia/NSW">>},
    {false, 36000, <<"Australia/Queensland">>},
    {false, 36000, <<"Australia/Sydney">>},
    {false, 36000, <<"Australia/Tasmania">>},
    {false, 36000, <<"Australia/Victoria">>}
];
timezone_info(<<"AFT">>) -> [
    {false, 16200, <<"Asia/Kabul">>},
    {false, 14400, <<"Asia/Kabul">>}
];
timezone_info(<<"AHDT">>) -> [
    {true, -32400, <<"America/Anchorage">>}
];
timezone_info(<<"AHST">>) -> [
    {false, -36000, <<"America/Anchorage">>},
    {false, -36000, <<"America/Adak">>},
    {false, -36000, <<"America/Atka">>}
];
timezone_info(<<"AKDT">>) -> [
    {true, -28800, <<"America/Anchorage">>},
    {true, -28800, <<"America/Juneau">>},
    {true, -28800, <<"America/Nome">>},
    {true, -28800, <<"America/Sitka">>},
    {true, -28800, <<"America/Yakutat">>}
];
timezone_info(<<"AKST">>) -> [
    {false, -32400, <<"America/Anchorage">>},
    {false, -32400, <<"America/Juneau">>},
    {false, -32400, <<"America/Nome">>},
    {false, -32400, <<"America/Sitka">>},
    {false, -32400, <<"America/Yakutat">>}
];
timezone_info(<<"AKTST">>) -> [
    {true, 21600, <<"Asia/Aqtobe">>}
];
timezone_info(<<"AKTT">>) -> [
    {false, 21600, <<"Asia/Aqtobe">>},
    {false, 14400, <<"Asia/Aqtobe">>},
    {false, 18000, <<"Asia/Aqtobe">>}
];
timezone_info(<<"ALMST">>) -> [
    {true, 25200, <<"Asia/Almaty">>}
];
timezone_info(<<"ALMT">>) -> [
    {false, 21600, <<"Asia/Almaty">>},
    {false, 18000, <<"Asia/Almaty">>}
];
timezone_info(<<"AMST">>) -> [
    {true, 18000, <<"Asia/Yerevan">>},
    {true, 14400, <<"Asia/Yerevan">>},
    {true, -10800, <<"America/Boa_Vista">>},
    {true, -10800, <<"America/Campo_Grande">>},
    {true, -10800, <<"America/Cuiaba">>},
    {true, -10800, <<"America/Manaus">>},
    {true, -10800, <<"America/Porto_Velho">>},
    {true, -10800, <<"America/Santarem">>},
    {true, -10800, <<"Brazil/West">>}
];
timezone_info(<<"AMT">>) -> [
    {false, 14400, <<"Asia/Yerevan">>},
    {false, 10800, <<"Asia/Yerevan">>},
    {false, -13840, <<"America/Asuncion">>},
    {false, -14400, <<"America/Boa_Vista">>},
    {false, -14400, <<"America/Campo_Grande">>},
    {false, -14400, <<"America/Cuiaba">>},
    {false, -14400, <<"America/Eirunepe">>},
    {false, -14400, <<"America/Manaus">>},
    {false, -14400, <<"America/Porto_Acre">>},
    {false, -14400, <<"America/Porto_Velho">>},
    {false, -14400, <<"America/Rio_Branco">>},
    {false, -14400, <<"America/Santarem">>},
    {false, -14400, <<"Brazil/Acre">>},
    {false, -14400, <<"Brazil/West">>},
    {false, 1172, <<"Europe/Amsterdam">>},
    {false, 5692, <<"Europe/Athens">>}
];
timezone_info(<<"ANAST">>) -> [
    {true, 50400, <<"Asia/Anadyr">>},
    {true, 43200, <<"Asia/Anadyr">>},
    {true, 46800, <<"Asia/Anadyr">>}
];
timezone_info(<<"ANAT">>) -> [
    {false, 46800, <<"Asia/Anadyr">>},
    {false, 39600, <<"Asia/Anadyr">>},
    {false, 43200, <<"Asia/Anadyr">>}
];
timezone_info(<<"ANT">>) -> [
    {false, -16200, <<"America/Curacao">>},
    {false, -16200, <<"America/Aruba">>},
    {false, -16200, <<"America/Kralendijk">>},
    {false, -16200, <<"America/Lower_Princes">>}
];
timezone_info(<<"AOT">>) -> [
    {false, 3124, <<"Africa/Luanda">>}
];
timezone_info(<<"APT">>) -> [
    {true, -10800, <<"America/Halifax">>},
    {true, -10800, <<"America/Blanc-Sablon">>},
    {true, -10800, <<"America/Glace_Bay">>},
    {true, -10800, <<"America/Moncton">>},
    {true, -10800, <<"America/Pangnirtung">>},
    {true, -10800, <<"America/Puerto_Rico">>},
    {true, -10800, <<"Canada/Atlantic">>}
];
timezone_info(<<"AQTST">>) -> [
    {true, 21600, <<"Asia/Aqtau">>},
    {true, 18000, <<"Asia/Aqtau">>},
    {true, 21600, <<"Asia/Aqtobe">>}
];
timezone_info(<<"AQTT">>) -> [
    {false, 18000, <<"Asia/Aqtau">>},
    {false, 14400, <<"Asia/Aqtau">>},
    {false, 18000, <<"Asia/Aqtobe">>}
];
timezone_info(<<"ARST">>) -> [
    {true, -7200, <<"America/Buenos_Aires">>},
    {true, -10800, <<"America/Buenos_Aires">>},
    {true, -10800, <<"America/Argentina/Buenos_Aires">>},
    {true, -10800, <<"America/Argentina/Catamarca">>},
    {true, -10800, <<"America/Argentina/ComodRivadavia">>},
    {true, -10800, <<"America/Argentina/Cordoba">>},
    {true, -10800, <<"America/Argentina/Jujuy">>},
    {true, -10800, <<"America/Argentina/La_Rioja">>},
    {true, -10800, <<"America/Argentina/Mendoza">>},
    {true, -10800, <<"America/Argentina/Rio_Gallegos">>},
    {true, -10800, <<"America/Argentina/Salta">>},
    {true, -10800, <<"America/Argentina/San_Juan">>},
    {true, -10800, <<"America/Argentina/San_Luis">>},
    {true, -10800, <<"America/Argentina/Tucuman">>},
    {true, -10800, <<"America/Argentina/Ushuaia">>},
    {true, -10800, <<"America/Catamarca">>},
    {true, -10800, <<"America/Cordoba">>},
    {true, -10800, <<"America/Jujuy">>},
    {true, -10800, <<"America/Mendoza">>},
    {true, -10800, <<"America/Rosario">>},
    {true, -10800, <<"Antarctica/Palmer">>},
    {true, -7200, <<"America/Argentina/Buenos_Aires">>},
    {true, -7200, <<"America/Argentina/Catamarca">>},
    {true, -7200, <<"America/Argentina/ComodRivadavia">>},
    {true, -7200, <<"America/Argentina/Cordoba">>},
    {true, -7200, <<"America/Argentina/Jujuy">>},
    {true, -7200, <<"America/Argentina/La_Rioja">>},
    {true, -7200, <<"America/Argentina/Mendoza">>},
    {true, -7200, <<"America/Argentina/Rio_Gallegos">>},
    {true, -7200, <<"America/Argentina/Salta">>},
    {true, -7200, <<"America/Argentina/San_Juan">>},
    {true, -7200, <<"America/Argentina/San_Luis">>},
    {true, -7200, <<"America/Argentina/Tucuman">>},
    {true, -7200, <<"America/Argentina/Ushuaia">>},
    {true, -7200, <<"America/Catamarca">>},
    {true, -7200, <<"America/Cordoba">>},
    {true, -7200, <<"America/Jujuy">>},
    {true, -7200, <<"America/Mendoza">>},
    {true, -7200, <<"America/Rosario">>},
    {true, -7200, <<"Antarctica/Palmer">>}
];
timezone_info(<<"ART">>) -> [
    {false, -10800, <<"America/Buenos_Aires">>},
    {false, -14400, <<"America/Buenos_Aires">>},
    {false, -10800, <<"America/Argentina/Buenos_Aires">>},
    {false, -10800, <<"America/Argentina/Catamarca">>},
    {false, -10800, <<"America/Argentina/ComodRivadavia">>},
    {false, -10800, <<"America/Argentina/Cordoba">>},
    {false, -10800, <<"America/Argentina/Jujuy">>},
    {false, -10800, <<"America/Argentina/La_Rioja">>},
    {false, -10800, <<"America/Argentina/Mendoza">>},
    {false, -10800, <<"America/Argentina/Rio_Gallegos">>},
    {false, -10800, <<"America/Argentina/Salta">>},
    {false, -10800, <<"America/Argentina/San_Juan">>},
    {false, -10800, <<"America/Argentina/San_Luis">>},
    {false, -10800, <<"America/Argentina/Tucuman">>},
    {false, -10800, <<"America/Argentina/Ushuaia">>},
    {false, -10800, <<"America/Catamarca">>},
    {false, -10800, <<"America/Cordoba">>},
    {false, -10800, <<"America/Jujuy">>},
    {false, -10800, <<"America/Mendoza">>},
    {false, -10800, <<"America/Rosario">>},
    {false, -10800, <<"Antarctica/Palmer">>},
    {false, -14400, <<"America/Argentina/Buenos_Aires">>},
    {false, -14400, <<"America/Argentina/Catamarca">>},
    {false, -14400, <<"America/Argentina/ComodRivadavia">>},
    {false, -14400, <<"America/Argentina/Cordoba">>},
    {false, -14400, <<"America/Argentina/Jujuy">>},
    {false, -14400, <<"America/Argentina/La_Rioja">>},
    {false, -14400, <<"America/Argentina/Mendoza">>},
    {false, -14400, <<"America/Argentina/Rio_Gallegos">>},
    {false, -14400, <<"America/Argentina/Salta">>},
    {false, -14400, <<"America/Argentina/San_Juan">>},
    {false, -14400, <<"America/Argentina/San_Luis">>},
    {false, -14400, <<"America/Argentina/Tucuman">>},
    {false, -14400, <<"America/Argentina/Ushuaia">>},
    {false, -14400, <<"America/Catamarca">>},
    {false, -14400, <<"America/Cordoba">>},
    {false, -14400, <<"America/Jujuy">>},
    {false, -14400, <<"America/Mendoza">>},
    {false, -14400, <<"America/Rosario">>},
    {false, -14400, <<"Antarctica/Palmer">>}
];
timezone_info(<<"ASHST">>) -> [
    {true, 21600, <<"Asia/Ashkhabad">>},
    {true, 18000, <<"Asia/Ashkhabad">>},
    {true, 18000, <<"Asia/Ashgabat">>},
    {true, 21600, <<"Asia/Ashgabat">>}
];
timezone_info(<<"ASHT">>) -> [
    {false, 18000, <<"Asia/Ashkhabad">>},
    {false, 14400, <<"Asia/Ashkhabad">>},
    {false, 14400, <<"Asia/Ashgabat">>},
    {false, 18000, <<"Asia/Ashgabat">>}
];
timezone_info(<<"AST">>) -> [
    {false, 10800, <<"Asia/Riyadh">>},
    {false, -14400, <<"America/Anguilla">>},
    {false, -14400, <<"America/Antigua">>},
    {false, -14400, <<"America/Aruba">>},
    {false, -14400, <<"America/Barbados">>},
    {false, -14400, <<"America/Blanc-Sablon">>},
    {false, -14400, <<"America/Curacao">>},
    {false, -14400, <<"America/Dominica">>},
    {false, -14400, <<"America/Glace_Bay">>},
    {false, -14400, <<"America/Goose_Bay">>},
    {false, -14400, <<"America/Grenada">>},
    {false, -14400, <<"America/Guadeloupe">>},
    {false, -14400, <<"America/Halifax">>},
    {false, -14400, <<"America/Kralendijk">>},
    {false, -14400, <<"America/Lower_Princes">>},
    {false, -14400, <<"America/Marigot">>},
    {false, -14400, <<"America/Martinique">>},
    {false, -14400, <<"America/Miquelon">>},
    {false, -14400, <<"America/Moncton">>},
    {false, -14400, <<"America/Montserrat">>},
    {false, -14400, <<"America/Pangnirtung">>},
    {false, -14400, <<"America/Port_of_Spain">>},
    {false, -14400, <<"America/Puerto_Rico">>},
    {false, -14400, <<"America/Santo_Domingo">>},
    {false, -14400, <<"America/St_Barthelemy">>},
    {false, -14400, <<"America/St_Kitts">>},
    {false, -14400, <<"America/St_Lucia">>},
    {false, -14400, <<"America/St_Thomas">>},
    {false, -14400, <<"America/St_Vincent">>},
    {false, -14400, <<"America/Thule">>},
    {false, -14400, <<"America/Tortola">>},
    {false, -14400, <<"America/Virgin">>},
    {false, -14400, <<"Atlantic/Bermuda">>},
    {false, -14400, <<"Canada/Atlantic">>},
    {false, 10800, <<"Asia/Aden">>},
    {false, 10800, <<"Asia/Baghdad">>},
    {false, 10800, <<"Asia/Bahrain">>},
    {false, 10800, <<"Asia/Kuwait">>},
    {false, 10800, <<"Asia/Qatar">>}
];
timezone_info(<<"AWDT">>) -> [
    {true, 32400, <<"Australia/Perth">>},
    {true, 32400, <<"Australia/West">>}
];
timezone_info(<<"AWST">>) -> [
    {false, 28800, <<"Australia/Perth">>},
    {false, 28800, <<"Antarctica/Casey">>},
    {false, 28800, <<"Australia/West">>}
];
timezone_info(<<"AWT">>) -> [
    {true, -10800, <<"America/Halifax">>},
    {true, -10800, <<"America/Blanc-Sablon">>},
    {true, -10800, <<"America/Glace_Bay">>},
    {true, -10800, <<"America/Moncton">>},
    {true, -10800, <<"America/Pangnirtung">>},
    {true, -10800, <<"America/Puerto_Rico">>},
    {true, -10800, <<"Canada/Atlantic">>}
];
timezone_info(<<"AZOMT">>) -> [
    {true, 0, <<"Atlantic/Azores">>}
];
timezone_info(<<"AZOST">>) -> [
    {true, 0, <<"Atlantic/Azores">>},
    {true, -3600, <<"Atlantic/Azores">>}
];
timezone_info(<<"AZOT">>) -> [
    {false, -3600, <<"Atlantic/Azores">>},
    {false, -7200, <<"Atlantic/Azores">>}
];
timezone_info(<<"AZST">>) -> [
    {true, 18000, <<"Asia/Baku">>},
    {true, 14400, <<"Asia/Baku">>}
];
timezone_info(<<"AZT">>) -> [
    {false, 14400, <<"Asia/Baku">>},
    {false, 10800, <<"Asia/Baku">>}
];
timezone_info(<<"BAKST">>) -> [
    {true, 18000, <<"Asia/Baku">>},
    {true, 14400, <<"Asia/Baku">>}
];
timezone_info(<<"BAKT">>) -> [
    {false, 14400, <<"Asia/Baku">>},
    {false, 10800, <<"Asia/Baku">>}
];
timezone_info(<<"BDST">>) -> [
    {true, 7200, <<"Europe/London">>},
    {true, 25200, <<"Asia/Dacca">>},
    {true, 25200, <<"Asia/Dhaka">>},
    {true, 7200, <<"Europe/Belfast">>},
    {true, 7200, <<"Europe/Gibraltar">>},
    {true, 7200, <<"Europe/Guernsey">>},
    {true, 7200, <<"Europe/Isle_of_Man">>},
    {true, 7200, <<"Europe/Jersey">>},
    {true, 7200, <<"GB">>}
];
timezone_info(<<"BDT">>) -> [
    {true, -36000, <<"America/Adak">>},
    {false, 21600, <<"Asia/Dacca">>},
    {true, -36000, <<"America/Atka">>},
    {true, -36000, <<"America/Nome">>},
    {false, 21600, <<"Asia/Dhaka">>}
];
timezone_info(<<"BEAT">>) -> [
    {false, 9000, <<"Africa/Mogadishu">>},
    {false, 9000, <<"Africa/Kampala">>},
    {false, 9000, <<"Africa/Nairobi">>}
];
timezone_info(<<"BEAUT">>) -> [
    {false, 9900, <<"Africa/Nairobi">>},
    {false, 9900, <<"Africa/Dar_es_Salaam">>},
    {false, 9900, <<"Africa/Kampala">>}
];
timezone_info(<<"BMT">>) -> [
    {false, -14309, <<"America/Barbados">>},
    {false, 6264, <<"Europe/Tiraspol">>},
    {false, -17776, <<"America/Bogota">>},
    {false, 10656, <<"Asia/Baghdad">>},
    {false, 24124, <<"Asia/Bangkok">>},
    {false, 25632, <<"Asia/Jakarta">>},
    {false, 6264, <<"Europe/Bucharest">>},
    {false, 6264, <<"Europe/Chisinau">>}
];
timezone_info(<<"BNT">>) -> [
    {false, 28800, <<"Asia/Brunei">>},
    {false, 27000, <<"Asia/Brunei">>}
];
timezone_info(<<"BORTST">>) -> [
    {true, 30000, <<"Asia/Kuching">>}
];
timezone_info(<<"BORT">>) -> [
    {false, 28800, <<"Asia/Kuching">>},
    {false, 27000, <<"Asia/Kuching">>}
];
timezone_info(<<"BOST">>) -> [
    {true, -12756, <<"America/La_Paz">>}
];
timezone_info(<<"BOT">>) -> [
    {false, -14400, <<"America/La_Paz">>}
];
timezone_info(<<"BRST">>) -> [
    {true, -7200, <<"America/Sao_Paulo">>},
    {true, -7200, <<"America/Araguaina">>},
    {true, -7200, <<"America/Bahia">>},
    {true, -7200, <<"America/Belem">>},
    {true, -7200, <<"America/Fortaleza">>},
    {true, -7200, <<"America/Maceio">>},
    {true, -7200, <<"America/Recife">>},
    {true, -7200, <<"Brazil/East">>}
];
timezone_info(<<"BRT">>) -> [
    {false, -10800, <<"America/Sao_Paulo">>},
    {false, -10800, <<"America/Araguaina">>},
    {false, -10800, <<"America/Bahia">>},
    {false, -10800, <<"America/Belem">>},
    {false, -10800, <<"America/Fortaleza">>},
    {false, -10800, <<"America/Maceio">>},
    {false, -10800, <<"America/Recife">>},
    {false, -10800, <<"America/Santarem">>},
    {false, -10800, <<"Brazil/East">>}
];
timezone_info(<<"BST">>) -> [
    {true, 3600, <<"Europe/London">>},
    {false, 3600, <<"Europe/London">>},
    {false, -39600, <<"America/Adak">>},
    {false, -39600, <<"America/Atka">>},
    {false, -39600, <<"America/Nome">>},
    {false, -39600, <<"Pacific/Midway">>},
    {false, -39600, <<"Pacific/Pago_Pago">>},
    {false, -39600, <<"Pacific/Samoa">>},
    {false, 3600, <<"Europe/Belfast">>},
    {false, 3600, <<"Europe/Guernsey">>},
    {false, 3600, <<"Europe/Isle_of_Man">>},
    {false, 3600, <<"Europe/Jersey">>},
    {false, 3600, <<"GB">>},
    {true, 3600, <<"Europe/Belfast">>},
    {true, 3600, <<"Europe/Dublin">>},
    {true, 3600, <<"Europe/Gibraltar">>},
    {true, 3600, <<"Europe/Guernsey">>},
    {true, 3600, <<"Europe/Isle_of_Man">>},
    {true, 3600, <<"Europe/Jersey">>},
    {true, 3600, <<"GB">>}
];
timezone_info(<<"BTT">>) -> [
    {false, 21600, <<"Asia/Thimbu">>},
    {false, 21600, <<"Asia/Thimphu">>}
];
timezone_info(<<"BURT">>) -> [
    {false, 23400, <<"Asia/Kolkata">>},
    {false, 23400, <<"Asia/Calcutta">>},
    {false, 23400, <<"Asia/Dacca">>},
    {false, 23400, <<"Asia/Dhaka">>},
    {false, 23400, <<"Asia/Rangoon">>}
];
timezone_info(<<"CANT">>) -> [
    {false, -3600, <<"Atlantic/Canary">>}
];
timezone_info(<<"CAPT">>) -> [
    {true, -32400, <<"America/Anchorage">>}
];
timezone_info(<<"CAST">>) -> [
    {false, 34200, <<"Australia/Adelaide">>},
    {true, 10800, <<"Africa/Gaborone">>},
    {true, 10800, <<"Africa/Juba">>},
    {true, 10800, <<"Africa/Khartoum">>},
    {false, 39600, <<"Antarctica/Casey">>}
];
timezone_info(<<"CAT">>) -> [
    {false, -36000, <<"America/Anchorage">>},
    {false, 7200, <<"Africa/Khartoum">>},
    {false, 7200, <<"Africa/Blantyre">>},
    {false, 7200, <<"Africa/Bujumbura">>},
    {false, 7200, <<"Africa/Gaborone">>},
    {false, 7200, <<"Africa/Harare">>},
    {false, 7200, <<"Africa/Juba">>},
    {false, 7200, <<"Africa/Kigali">>},
    {false, 7200, <<"Africa/Lubumbashi">>},
    {false, 7200, <<"Africa/Lusaka">>},
    {false, 7200, <<"Africa/Maputo">>},
    {false, 7200, <<"Africa/Windhoek">>}
];
timezone_info(<<"CAWT">>) -> [
    {true, -32400, <<"America/Anchorage">>}
];
timezone_info(<<"CCT">>) -> [
    {false, 23400, <<"Indian/Cocos">>}
];
timezone_info(<<"CDDT">>) -> [
    {true, -14400, <<"America/Rankin_Inlet">>},
    {true, -14400, <<"America/Resolute">>}
];
timezone_info(<<"CDT">>) -> [
    {true, -18000, <<"America/Chicago">>},
    {true, 32400, <<"Asia/Shanghai">>},
    {true, -14400, <<"America/Havana">>},
    {true, -18000, <<"America/Atikokan">>},
    {true, -18000, <<"America/Bahia_Banderas">>},
    {true, -18000, <<"America/Belize">>},
    {true, -18000, <<"America/Cambridge_Bay">>},
    {true, -18000, <<"America/Cancun">>},
    {true, -18000, <<"America/Chihuahua">>},
    {true, -18000, <<"America/Coral_Harbour">>},
    {true, -18000, <<"America/Costa_Rica">>},
    {true, -18000, <<"America/El_Salvador">>},
    {true, -18000, <<"America/Fort_Wayne">>},
    {true, -18000, <<"America/Guatemala">>},
    {true, -18000, <<"America/Indiana/Indianapolis">>},
    {true, -18000, <<"America/Indiana/Knox">>},
    {true, -18000, <<"America/Indiana/Marengo">>},
    {true, -18000, <<"America/Indiana/Petersburg">>},
    {true, -18000, <<"America/Indiana/Tell_City">>},
    {true, -18000, <<"America/Indiana/Vevay">>},
    {true, -18000, <<"America/Indiana/Vincennes">>},
    {true, -18000, <<"America/Indiana/Winamac">>},
    {true, -18000, <<"America/Indianapolis">>},
    {true, -18000, <<"America/Iqaluit">>},
    {true, -18000, <<"America/Kentucky/Louisville">>},
    {true, -18000, <<"America/Kentucky/Monticello">>},
    {true, -18000, <<"America/Knox_IN">>},
    {true, -18000, <<"America/Louisville">>},
    {true, -18000, <<"America/Managua">>},
    {true, -18000, <<"America/Matamoros">>},
    {true, -18000, <<"America/Menominee">>},
    {true, -18000, <<"America/Merida">>},
    {true, -18000, <<"America/Mexico_City">>},
    {true, -18000, <<"America/Monterrey">>},
    {true, -18000, <<"America/North_Dakota/Beulah">>},
    {true, -18000, <<"America/North_Dakota/Center">>},
    {true, -18000, <<"America/North_Dakota/New_Salem">>},
    {true, -18000, <<"America/Ojinaga">>},
    {true, -18000, <<"America/Pangnirtung">>},
    {true, -18000, <<"America/Rainy_River">>},
    {true, -18000, <<"America/Rankin_Inlet">>},
    {true, -18000, <<"America/Resolute">>},
    {true, -18000, <<"America/Tegucigalpa">>},
    {true, -18000, <<"America/Winnipeg">>},
    {true, -18000, <<"Canada/Central">>},
    {true, -18000, <<"Mexico/General">>},
    {true, 32400, <<"Asia/Chongqing">>},
    {true, 32400, <<"Asia/Chungking">>},
    {true, 32400, <<"Asia/Harbin">>},
    {true, 32400, <<"Asia/Taipei">>},
    {true, 32400, <<"PRC">>},
    {true, 32400, <<"ROC">>}
];
timezone_info(<<"CEMT">>) -> [
    {true, 10800, <<"Europe/Berlin">>},
    {true, 10800, <<"CET">>}
];
timezone_info(<<"CEST">>) -> [
    {true, 7200, <<"Europe/Berlin">>},
    {true, 10800, <<"Europe/Kaliningrad">>},
    {true, 7200, <<"Africa/Algiers">>},
    {true, 7200, <<"Africa/Ceuta">>},
    {true, 7200, <<"Africa/Tripoli">>},
    {true, 7200, <<"Africa/Tunis">>},
    {true, 7200, <<"Antarctica/Troll">>},
    {true, 7200, <<"Arctic/Longyearbyen">>},
    {true, 7200, <<"Atlantic/Jan_Mayen">>},
    {true, 7200, <<"CET">>},
    {true, 7200, <<"Europe/Amsterdam">>},
    {true, 7200, <<"Europe/Andorra">>},
    {true, 7200, <<"Europe/Athens">>},
    {true, 7200, <<"Europe/Belgrade">>},
    {true, 7200, <<"Europe/Bratislava">>},
    {true, 7200, <<"Europe/Brussels">>},
    {true, 7200, <<"Europe/Budapest">>},
    {true, 7200, <<"Europe/Busingen">>},
    {true, 7200, <<"Europe/Chisinau">>},
    {true, 7200, <<"Europe/Copenhagen">>},
    {true, 7200, <<"Europe/Gibraltar">>},
    {true, 7200, <<"Europe/Kaliningrad">>},
    {true, 7200, <<"Europe/Kiev">>},
    {true, 7200, <<"Europe/Lisbon">>},
    {true, 7200, <<"Europe/Ljubljana">>},
    {true, 7200, <<"Europe/Luxembourg">>},
    {true, 7200, <<"Europe/Madrid">>},
    {true, 7200, <<"Europe/Malta">>},
    {true, 7200, <<"Europe/Minsk">>},
    {true, 7200, <<"Europe/Monaco">>},
    {true, 7200, <<"Europe/Oslo">>},
    {true, 7200, <<"Europe/Paris">>},
    {true, 7200, <<"Europe/Podgorica">>},
    {true, 7200, <<"Europe/Prague">>},
    {true, 7200, <<"Europe/Riga">>},
    {true, 7200, <<"Europe/Rome">>},
    {true, 7200, <<"Europe/San_Marino">>},
    {true, 7200, <<"Europe/Sarajevo">>},
    {true, 7200, <<"Europe/Simferopol">>},
    {true, 7200, <<"Europe/Skopje">>},
    {true, 7200, <<"Europe/Sofia">>},
    {true, 7200, <<"Europe/Stockholm">>},
    {true, 7200, <<"Europe/Tallinn">>},
    {true, 7200, <<"Europe/Tirane">>},
    {true, 7200, <<"Europe/Tiraspol">>},
    {true, 7200, <<"Europe/Uzhgorod">>},
    {true, 7200, <<"Europe/Vaduz">>},
    {true, 7200, <<"Europe/Vatican">>},
    {true, 7200, <<"Europe/Vienna">>},
    {true, 7200, <<"Europe/Vilnius">>},
    {true, 7200, <<"Europe/Warsaw">>},
    {true, 7200, <<"Europe/Zagreb">>},
    {true, 7200, <<"Europe/Zaporozhye">>},
    {true, 7200, <<"Europe/Zurich">>},
    {true, 7200, <<"WET">>}
];
timezone_info(<<"CET">>) -> [
    {false, 3600, <<"Europe/Berlin">>},
    {false, 7200, <<"Europe/Kaliningrad">>},
    {false, 3600, <<"Africa/Algiers">>},
    {false, 3600, <<"Africa/Casablanca">>},
    {false, 3600, <<"Africa/Ceuta">>},
    {false, 3600, <<"Africa/Tripoli">>},
    {false, 3600, <<"Africa/Tunis">>},
    {false, 3600, <<"Arctic/Longyearbyen">>},
    {false, 3600, <<"Atlantic/Jan_Mayen">>},
    {false, 3600, <<"CET">>},
    {false, 3600, <<"Europe/Amsterdam">>},
    {false, 3600, <<"Europe/Andorra">>},
    {false, 3600, <<"Europe/Athens">>},
    {false, 3600, <<"Europe/Belgrade">>},
    {false, 3600, <<"Europe/Bratislava">>},
    {false, 3600, <<"Europe/Brussels">>},
    {false, 3600, <<"Europe/Budapest">>},
    {false, 3600, <<"Europe/Busingen">>},
    {false, 3600, <<"Europe/Chisinau">>},
    {false, 3600, <<"Europe/Copenhagen">>},
    {false, 3600, <<"Europe/Gibraltar">>},
    {false, 3600, <<"Europe/Kaliningrad">>},
    {false, 3600, <<"Europe/Kiev">>},
    {false, 3600, <<"Europe/Lisbon">>},
    {false, 3600, <<"Europe/Ljubljana">>},
    {false, 3600, <<"Europe/Luxembourg">>},
    {false, 3600, <<"Europe/Madrid">>},
    {false, 3600, <<"Europe/Malta">>},
    {false, 3600, <<"Europe/Minsk">>},
    {false, 3600, <<"Europe/Monaco">>},
    {false, 3600, <<"Europe/Oslo">>},
    {false, 3600, <<"Europe/Paris">>},
    {false, 3600, <<"Europe/Podgorica">>},
    {false, 3600, <<"Europe/Prague">>},
    {false, 3600, <<"Europe/Riga">>},
    {false, 3600, <<"Europe/Rome">>},
    {false, 3600, <<"Europe/San_Marino">>},
    {false, 3600, <<"Europe/Sarajevo">>},
    {false, 3600, <<"Europe/Simferopol">>},
    {false, 3600, <<"Europe/Skopje">>},
    {false, 3600, <<"Europe/Sofia">>},
    {false, 3600, <<"Europe/Stockholm">>},
    {false, 3600, <<"Europe/Tallinn">>},
    {false, 3600, <<"Europe/Tirane">>},
    {false, 3600, <<"Europe/Tiraspol">>},
    {false, 3600, <<"Europe/Uzhgorod">>},
    {false, 3600, <<"Europe/Vaduz">>},
    {false, 3600, <<"Europe/Vatican">>},
    {false, 3600, <<"Europe/Vienna">>},
    {false, 3600, <<"Europe/Vilnius">>},
    {false, 3600, <<"Europe/Warsaw">>},
    {false, 3600, <<"Europe/Zagreb">>},
    {false, 3600, <<"Europe/Zaporozhye">>},
    {false, 3600, <<"Europe/Zurich">>},
    {false, 3600, <<"WET">>}
];
timezone_info(<<"CGST">>) -> [
    {true, -3600, <<"America/Scoresbysund">>}
];
timezone_info(<<"CGT">>) -> [
    {false, -7200, <<"America/Scoresbysund">>}
];
timezone_info(<<"CHADT">>) -> [
    {true, 49500, <<"Pacific/Chatham">>}
];
timezone_info(<<"CHAST">>) -> [
    {false, 45900, <<"Pacific/Chatham">>},
    {false, 44100, <<"Pacific/Chatham">>}
];
timezone_info(<<"CHDT">>) -> [
    {true, -19800, <<"America/Belize">>}
];
timezone_info(<<"CHOST">>) -> [
    {true, 36000, <<"Asia/Choibalsan">>}
];
timezone_info(<<"CHOT">>) -> [
    {false, 32400, <<"Asia/Choibalsan">>},
    {false, 28800, <<"Asia/Choibalsan">>}
];
timezone_info(<<"CHUT">>) -> [
    {false, 36000, <<"Pacific/Chuuk">>},
    {false, 36000, <<"Pacific/Truk">>},
    {false, 36000, <<"Pacific/Yap">>}
];
timezone_info(<<"CKHST">>) -> [
    {true, -34200, <<"Pacific/Rarotonga">>}
];
timezone_info(<<"CKT">>) -> [
    {false, -36000, <<"Pacific/Rarotonga">>},
    {false, -37800, <<"Pacific/Rarotonga">>}
];
timezone_info(<<"CLST">>) -> [
    {true, -10800, <<"America/Santiago">>},
    {true, -14400, <<"America/Santiago">>},
    {true, -10800, <<"Antarctica/Palmer">>},
    {true, -10800, <<"Chile/Continental">>},
    {true, -14400, <<"Chile/Continental">>}
];
timezone_info(<<"CLT">>) -> [
    {false, -14400, <<"America/Santiago">>},
    {false, -18000, <<"America/Santiago">>},
    {false, -14400, <<"Antarctica/Palmer">>},
    {false, -14400, <<"Chile/Continental">>},
    {false, -18000, <<"Chile/Continental">>}
];
timezone_info(<<"CMT">>) -> [
    {false, -15408, <<"America/Argentina/Buenos_Aires">>},
    {false, -15408, <<"America/Argentina/Catamarca">>},
    {false, -15408, <<"America/Argentina/ComodRivadavia">>},
    {false, -15408, <<"America/Argentina/Cordoba">>},
    {false, -15408, <<"America/Argentina/Jujuy">>},
    {false, -15408, <<"America/Argentina/La_Rioja">>},
    {false, -15408, <<"America/Argentina/Mendoza">>},
    {false, -15408, <<"America/Argentina/Rio_Gallegos">>},
    {false, -15408, <<"America/Argentina/Salta">>},
    {false, -15408, <<"America/Argentina/San_Juan">>},
    {false, -15408, <<"America/Argentina/San_Luis">>},
    {false, -15408, <<"America/Argentina/Tucuman">>},
    {false, -15408, <<"America/Argentina/Ushuaia">>},
    {false, -15408, <<"America/Buenos_Aires">>},
    {false, -15408, <<"America/Catamarca">>},
    {false, -15408, <<"America/Cordoba">>},
    {false, -15408, <<"America/Jujuy">>},
    {false, -15408, <<"America/Mendoza">>},
    {false, -15408, <<"America/Rosario">>},
    {false, -16060, <<"America/Caracas">>},
    {false, -16356, <<"America/La_Paz">>},
    {false, -19176, <<"America/Panama">>},
    {false, 6900, <<"Europe/Chisinau">>},
    {false, 6900, <<"Europe/Tiraspol">>}
];
timezone_info(<<"COST">>) -> [
    {true, -14400, <<"America/Bogota">>}
];
timezone_info(<<"COT">>) -> [
    {false, -18000, <<"America/Bogota">>}
];
timezone_info(<<"CPT">>) -> [
    {true, -18000, <<"America/Chicago">>},
    {true, -18000, <<"America/Atikokan">>},
    {true, -18000, <<"America/Coral_Harbour">>},
    {true, -18000, <<"America/Fort_Wayne">>},
    {true, -18000, <<"America/Indiana/Indianapolis">>},
    {true, -18000, <<"America/Indiana/Knox">>},
    {true, -18000, <<"America/Indiana/Marengo">>},
    {true, -18000, <<"America/Indiana/Petersburg">>},
    {true, -18000, <<"America/Indiana/Tell_City">>},
    {true, -18000, <<"America/Indiana/Vevay">>},
    {true, -18000, <<"America/Indiana/Vincennes">>},
    {true, -18000, <<"America/Indiana/Winamac">>},
    {true, -18000, <<"America/Indianapolis">>},
    {true, -18000, <<"America/Kentucky/Louisville">>},
    {true, -18000, <<"America/Kentucky/Monticello">>},
    {true, -18000, <<"America/Knox_IN">>},
    {true, -18000, <<"America/Louisville">>},
    {true, -18000, <<"America/Menominee">>},
    {true, -18000, <<"America/Rainy_River">>},
    {true, -18000, <<"America/Winnipeg">>},
    {true, -18000, <<"Canada/Central">>}
];
timezone_info(<<"CST">>) -> [
    {false, -21600, <<"America/Chicago">>},
    {false, -18000, <<"America/Havana">>},
    {false, -21600, <<"America/Atikokan">>},
    {false, -21600, <<"America/Bahia_Banderas">>},
    {false, -21600, <<"America/Belize">>},
    {false, -21600, <<"America/Cambridge_Bay">>},
    {false, -21600, <<"America/Cancun">>},
    {false, -21600, <<"America/Chihuahua">>},
    {false, -21600, <<"America/Coral_Harbour">>},
    {false, -21600, <<"America/Costa_Rica">>},
    {false, -21600, <<"America/Detroit">>},
    {false, -21600, <<"America/El_Salvador">>},
    {false, -21600, <<"America/Fort_Wayne">>},
    {false, -21600, <<"America/Guatemala">>},
    {false, -21600, <<"America/Hermosillo">>},
    {false, -21600, <<"America/Indiana/Indianapolis">>},
    {false, -21600, <<"America/Indiana/Knox">>},
    {false, -21600, <<"America/Indiana/Marengo">>},
    {false, -21600, <<"America/Indiana/Petersburg">>},
    {false, -21600, <<"America/Indiana/Tell_City">>},
    {false, -21600, <<"America/Indiana/Vevay">>},
    {false, -21600, <<"America/Indiana/Vincennes">>},
    {false, -21600, <<"America/Indiana/Winamac">>},
    {false, -21600, <<"America/Indianapolis">>},
    {false, -21600, <<"America/Iqaluit">>},
    {false, -21600, <<"America/Kentucky/Louisville">>},
    {false, -21600, <<"America/Kentucky/Monticello">>},
    {false, -21600, <<"America/Knox_IN">>},
    {false, -21600, <<"America/Louisville">>},
    {false, -21600, <<"America/Managua">>},
    {false, -21600, <<"America/Matamoros">>},
    {false, -21600, <<"America/Mazatlan">>},
    {false, -21600, <<"America/Menominee">>},
    {false, -21600, <<"America/Merida">>},
    {false, -21600, <<"America/Mexico_City">>},
    {false, -21600, <<"America/Monterrey">>},
    {false, -21600, <<"America/North_Dakota/Beulah">>},
    {false, -21600, <<"America/North_Dakota/Center">>},
    {false, -21600, <<"America/North_Dakota/New_Salem">>},
    {false, -21600, <<"America/Ojinaga">>},
    {false, -21600, <<"America/Pangnirtung">>},
    {false, -21600, <<"America/Rainy_River">>},
    {false, -21600, <<"America/Rankin_Inlet">>},
    {false, -21600, <<"America/Regina">>},
    {false, -21600, <<"America/Resolute">>},
    {false, -21600, <<"America/Swift_Current">>},
    {false, -21600, <<"America/Tegucigalpa">>},
    {false, -21600, <<"America/Thunder_Bay">>},
    {false, -21600, <<"America/Winnipeg">>},
    {false, -21600, <<"Canada/Central">>},
    {false, -21600, <<"Canada/East-Saskatchewan">>},
    {false, -21600, <<"Canada/Saskatchewan">>},
    {false, -21600, <<"Mexico/BajaSur">>},
    {false, -21600, <<"Mexico/General">>},
    {false, 28800, <<"Asia/Chongqing">>},
    {false, 28800, <<"Asia/Chungking">>},
    {false, 28800, <<"Asia/Harbin">>},
    {false, 28800, <<"Asia/Macao">>},
    {false, 28800, <<"Asia/Macau">>},
    {false, 28800, <<"Asia/Shanghai">>},
    {false, 28800, <<"Asia/Taipei">>},
    {false, 28800, <<"PRC">>},
    {false, 28800, <<"ROC">>}
];
timezone_info(<<"CUT">>) -> [
    {false, 8400, <<"Europe/Zaporozhye">>}
];
timezone_info(<<"CVST">>) -> [
    {true, -3600, <<"Atlantic/Cape_Verde">>}
];
timezone_info(<<"CVT">>) -> [
    {false, -7200, <<"Atlantic/Cape_Verde">>},
    {false, -3600, <<"Atlantic/Cape_Verde">>}
];
timezone_info(<<"CWT">>) -> [
    {true, -18000, <<"America/Chicago">>},
    {true, -18000, <<"America/Atikokan">>},
    {true, -18000, <<"America/Coral_Harbour">>},
    {true, -18000, <<"America/Fort_Wayne">>},
    {true, -18000, <<"America/Indiana/Indianapolis">>},
    {true, -18000, <<"America/Indiana/Knox">>},
    {true, -18000, <<"America/Indiana/Marengo">>},
    {true, -18000, <<"America/Indiana/Petersburg">>},
    {true, -18000, <<"America/Indiana/Tell_City">>},
    {true, -18000, <<"America/Indiana/Vevay">>},
    {true, -18000, <<"America/Indiana/Vincennes">>},
    {true, -18000, <<"America/Indiana/Winamac">>},
    {true, -18000, <<"America/Indianapolis">>},
    {true, -18000, <<"America/Kentucky/Louisville">>},
    {true, -18000, <<"America/Kentucky/Monticello">>},
    {true, -18000, <<"America/Knox_IN">>},
    {true, -18000, <<"America/Louisville">>},
    {true, -18000, <<"America/Menominee">>},
    {true, -18000, <<"America/Mexico_City">>},
    {true, -18000, <<"America/Rainy_River">>},
    {true, -18000, <<"America/Winnipeg">>},
    {true, -18000, <<"Canada/Central">>},
    {true, -18000, <<"Mexico/General">>}
];
timezone_info(<<"CXT">>) -> [
    {false, 25200, <<"Indian/Christmas">>}
];
timezone_info(<<"CHST">>) -> [
    {false, 36000, <<"Pacific/Guam">>},
    {false, 36000, <<"Pacific/Saipan">>}
];
timezone_info(<<"DACT">>) -> [
    {false, 21600, <<"Asia/Dacca">>},
    {false, 21600, <<"Asia/Dhaka">>}
];
timezone_info(<<"DAVT">>) -> [
    {false, 25200, <<"Antarctica/Davis">>},
    {false, 18000, <<"Antarctica/Davis">>}
];
timezone_info(<<"DDUT">>) -> [
    {false, 36000, <<"Antarctica/DumontDUrville">>}
];
timezone_info(<<"DMT">>) -> [
    {false, -1521, <<"Europe/Dublin">>}
];
timezone_info(<<"DUSST">>) -> [
    {true, 25200, <<"Asia/Dushanbe">>},
    {true, 21600, <<"Asia/Dushanbe">>}
];
timezone_info(<<"DUST">>) -> [
    {false, 21600, <<"Asia/Dushanbe">>},
    {false, 18000, <<"Asia/Dushanbe">>}
];
timezone_info(<<"EASST">>) -> [
    {true, -21600, <<"Chile/EasterIsland">>},
    {true, -18000, <<"Chile/EasterIsland">>},
    {true, -18000, <<"Pacific/Easter">>},
    {true, -21600, <<"Pacific/Easter">>}
];
timezone_info(<<"EAST">>) -> [
    {false, -21600, <<"Chile/EasterIsland">>},
    {false, -25200, <<"Chile/EasterIsland">>},
    {true, 14400, <<"Indian/Antananarivo">>},
    {false, -21600, <<"Pacific/Easter">>},
    {false, -25200, <<"Pacific/Easter">>}
];
timezone_info(<<"EAT">>) -> [
    {false, 10800, <<"Africa/Khartoum">>},
    {false, 10800, <<"Africa/Addis_Ababa">>},
    {false, 10800, <<"Africa/Asmara">>},
    {false, 10800, <<"Africa/Asmera">>},
    {false, 10800, <<"Africa/Dar_es_Salaam">>},
    {false, 10800, <<"Africa/Djibouti">>},
    {false, 10800, <<"Africa/Juba">>},
    {false, 10800, <<"Africa/Kampala">>},
    {false, 10800, <<"Africa/Mogadishu">>},
    {false, 10800, <<"Africa/Nairobi">>},
    {false, 10800, <<"Indian/Antananarivo">>},
    {false, 10800, <<"Indian/Comoro">>},
    {false, 10800, <<"Indian/Mayotte">>}
];
timezone_info(<<"ECT">>) -> [
    {false, -18000, <<"America/Guayaquil">>},
    {false, -18000, <<"Pacific/Galapagos">>}
];
timezone_info(<<"EDDT">>) -> [
    {true, -10800, <<"America/Iqaluit">>}
];
timezone_info(<<"EDT">>) -> [
    {true, -14400, <<"America/New_York">>},
    {true, -14400, <<"America/Cancun">>},
    {true, -14400, <<"America/Detroit">>},
    {true, -14400, <<"America/Fort_Wayne">>},
    {true, -14400, <<"America/Grand_Turk">>},
    {true, -14400, <<"America/Indiana/Indianapolis">>},
    {true, -14400, <<"America/Indiana/Marengo">>},
    {true, -14400, <<"America/Indiana/Petersburg">>},
    {true, -14400, <<"America/Indiana/Tell_City">>},
    {true, -14400, <<"America/Indiana/Vevay">>},
    {true, -14400, <<"America/Indiana/Vincennes">>},
    {true, -14400, <<"America/Indiana/Winamac">>},
    {true, -14400, <<"America/Indianapolis">>},
    {true, -14400, <<"America/Iqaluit">>},
    {true, -14400, <<"America/Jamaica">>},
    {true, -14400, <<"America/Kentucky/Louisville">>},
    {true, -14400, <<"America/Kentucky/Monticello">>},
    {true, -14400, <<"America/Louisville">>},
    {true, -14400, <<"America/Montreal">>},
    {true, -14400, <<"America/Nassau">>},
    {true, -14400, <<"America/Nipigon">>},
    {true, -14400, <<"America/Pangnirtung">>},
    {true, -14400, <<"America/Port-au-Prince">>},
    {true, -14400, <<"America/Santo_Domingo">>},
    {true, -14400, <<"America/Thunder_Bay">>},
    {true, -14400, <<"America/Toronto">>},
    {true, -14400, <<"Canada/Eastern">>},
    {true, -14400, <<"EST">>}
];
timezone_info(<<"EEST">>) -> [
    {true, 10800, <<"Europe/Helsinki">>},
    {true, 10800, <<"Africa/Cairo">>},
    {true, 10800, <<"Asia/Amman">>},
    {true, 10800, <<"Asia/Beirut">>},
    {true, 10800, <<"Asia/Damascus">>},
    {true, 10800, <<"Asia/Gaza">>},
    {true, 10800, <<"Asia/Hebron">>},
    {true, 10800, <<"Asia/Istanbul">>},
    {true, 10800, <<"Asia/Nicosia">>},
    {true, 10800, <<"EET">>},
    {true, 10800, <<"Europe/Athens">>},
    {true, 10800, <<"Europe/Bucharest">>},
    {true, 10800, <<"Europe/Chisinau">>},
    {true, 10800, <<"Europe/Istanbul">>},
    {true, 10800, <<"Europe/Kaliningrad">>},
    {true, 10800, <<"Europe/Kiev">>},
    {true, 10800, <<"Europe/Mariehamn">>},
    {true, 10800, <<"Europe/Minsk">>},
    {true, 10800, <<"Europe/Moscow">>},
    {true, 10800, <<"Europe/Nicosia">>},
    {true, 10800, <<"Europe/Riga">>},
    {true, 10800, <<"Europe/Samara">>},
    {true, 10800, <<"Europe/Simferopol">>},
    {true, 10800, <<"Europe/Sofia">>},
    {true, 10800, <<"Europe/Tallinn">>},
    {true, 10800, <<"Europe/Tiraspol">>},
    {true, 10800, <<"Europe/Uzhgorod">>},
    {true, 10800, <<"Europe/Vilnius">>},
    {true, 10800, <<"Europe/Warsaw">>},
    {true, 10800, <<"Europe/Zaporozhye">>}
];
timezone_info(<<"EET">>) -> [
    {false, 7200, <<"Europe/Helsinki">>},
    {true, 10800, <<"Asia/Gaza">>},
    {true, 10800, <<"Asia/Hebron">>},
    {false, 7200, <<"Africa/Cairo">>},
    {false, 7200, <<"Africa/Tripoli">>},
    {false, 7200, <<"Asia/Amman">>},
    {false, 7200, <<"Asia/Beirut">>},
    {false, 7200, <<"Asia/Damascus">>},
    {false, 7200, <<"Asia/Gaza">>},
    {false, 7200, <<"Asia/Hebron">>},
    {false, 7200, <<"Asia/Istanbul">>},
    {false, 7200, <<"Asia/Nicosia">>},
    {false, 7200, <<"EET">>},
    {false, 7200, <<"Europe/Athens">>},
    {false, 7200, <<"Europe/Bucharest">>},
    {false, 7200, <<"Europe/Chisinau">>},
    {false, 7200, <<"Europe/Istanbul">>},
    {false, 7200, <<"Europe/Kaliningrad">>},
    {false, 7200, <<"Europe/Kiev">>},
    {false, 7200, <<"Europe/Mariehamn">>},
    {false, 7200, <<"Europe/Minsk">>},
    {false, 7200, <<"Europe/Moscow">>},
    {false, 7200, <<"Europe/Nicosia">>},
    {false, 7200, <<"Europe/Riga">>},
    {false, 7200, <<"Europe/Simferopol">>},
    {false, 7200, <<"Europe/Sofia">>},
    {false, 7200, <<"Europe/Tallinn">>},
    {false, 7200, <<"Europe/Tiraspol">>},
    {false, 7200, <<"Europe/Uzhgorod">>},
    {false, 7200, <<"Europe/Vilnius">>},
    {false, 7200, <<"Europe/Warsaw">>},
    {false, 7200, <<"Europe/Zaporozhye">>}
];
timezone_info(<<"EGST">>) -> [
    {true, 0, <<"America/Scoresbysund">>}
];
timezone_info(<<"EGT">>) -> [
    {false, -3600, <<"America/Scoresbysund">>}
];
timezone_info(<<"EHDT">>) -> [
    {true, -16200, <<"America/Santo_Domingo">>}
];
timezone_info(<<"EMT">>) -> [
    {false, -26248, <<"Chile/EasterIsland">>},
    {false, -26248, <<"Pacific/Easter">>}
];
timezone_info(<<"EPT">>) -> [
    {true, -14400, <<"America/New_York">>},
    {true, -14400, <<"America/Detroit">>},
    {true, -14400, <<"America/Iqaluit">>},
    {true, -14400, <<"America/Montreal">>},
    {true, -14400, <<"America/Nipigon">>},
    {true, -14400, <<"America/Thunder_Bay">>},
    {true, -14400, <<"America/Toronto">>},
    {true, -14400, <<"Canada/Eastern">>},
    {true, -14400, <<"EST">>}
];
timezone_info(<<"EST">>) -> [
    {false, -18000, <<"America/New_York">>},
    {false, -18000, <<"America/Antigua">>},
    {false, -18000, <<"America/Atikokan">>},
    {false, -18000, <<"America/Cambridge_Bay">>},
    {false, -18000, <<"America/Cancun">>},
    {false, -18000, <<"America/Cayman">>},
    {false, -18000, <<"America/Chicago">>},
    {false, -18000, <<"America/Coral_Harbour">>},
    {false, -18000, <<"America/Detroit">>},
    {false, -18000, <<"America/Fort_Wayne">>},
    {false, -18000, <<"America/Grand_Turk">>},
    {false, -18000, <<"America/Indiana/Indianapolis">>},
    {false, -18000, <<"America/Indiana/Knox">>},
    {false, -18000, <<"America/Indiana/Marengo">>},
    {false, -18000, <<"America/Indiana/Petersburg">>},
    {false, -18000, <<"America/Indiana/Tell_City">>},
    {false, -18000, <<"America/Indiana/Vevay">>},
    {false, -18000, <<"America/Indiana/Vincennes">>},
    {false, -18000, <<"America/Indiana/Winamac">>},
    {false, -18000, <<"America/Indianapolis">>},
    {false, -18000, <<"America/Iqaluit">>},
    {false, -18000, <<"America/Jamaica">>},
    {false, -18000, <<"America/Kentucky/Louisville">>},
    {false, -18000, <<"America/Kentucky/Monticello">>},
    {false, -18000, <<"America/Knox_IN">>},
    {false, -18000, <<"America/Louisville">>},
    {false, -18000, <<"America/Managua">>},
    {false, -18000, <<"America/Menominee">>},
    {false, -18000, <<"America/Merida">>},
    {false, -18000, <<"America/Moncton">>},
    {false, -18000, <<"America/Montreal">>},
    {false, -18000, <<"America/Nassau">>},
    {false, -18000, <<"America/Nipigon">>},
    {false, -18000, <<"America/Panama">>},
    {false, -18000, <<"America/Pangnirtung">>},
    {false, -18000, <<"America/Port-au-Prince">>},
    {false, -18000, <<"America/Rankin_Inlet">>},
    {false, -18000, <<"America/Resolute">>},
    {false, -18000, <<"America/Santo_Domingo">>},
    {false, -18000, <<"America/Thunder_Bay">>},
    {false, -18000, <<"America/Toronto">>},
    {false, -18000, <<"Canada/Eastern">>},
    {false, -18000, <<"EST">>}
];
timezone_info(<<"EWT">>) -> [
    {true, -14400, <<"America/New_York">>},
    {true, -14400, <<"America/Detroit">>},
    {true, -14400, <<"America/Iqaluit">>},
    {true, -14400, <<"America/Montreal">>},
    {true, -14400, <<"America/Nipigon">>},
    {true, -14400, <<"America/Thunder_Bay">>},
    {true, -14400, <<"America/Toronto">>},
    {true, -14400, <<"Canada/Eastern">>},
    {true, -14400, <<"EST">>}
];
timezone_info(<<"FET">>) -> [
    {false, 10800, <<"Europe/Kaliningrad">>},
    {false, 10800, <<"Europe/Minsk">>}
];
timezone_info(<<"FFMT">>) -> [
    {false, -14660, <<"America/Martinique">>}
];
timezone_info(<<"FJST">>) -> [
    {true, 46800, <<"Pacific/Fiji">>}
];
timezone_info(<<"FJT">>) -> [
    {false, 43200, <<"Pacific/Fiji">>}
];
timezone_info(<<"FKST">>) -> [
    {true, -7200, <<"Atlantic/Stanley">>},
    {false, -10800, <<"Atlantic/Stanley">>},
    {true, -10800, <<"Atlantic/Stanley">>}
];
timezone_info(<<"FKT">>) -> [
    {false, -14400, <<"Atlantic/Stanley">>},
    {false, -10800, <<"Atlantic/Stanley">>}
];
timezone_info(<<"FMT">>) -> [
    {false, -4056, <<"Atlantic/Madeira">>}
];
timezone_info(<<"FNST">>) -> [
    {true, -3600, <<"America/Noronha">>},
    {true, -3600, <<"Brazil/DeNoronha">>}
];
timezone_info(<<"FNT">>) -> [
    {false, -7200, <<"America/Noronha">>},
    {false, -7200, <<"Brazil/DeNoronha">>}
];
timezone_info(<<"FORT">>) -> [
    {false, 18000, <<"Asia/Aqtau">>},
    {false, 14400, <<"Asia/Aqtau">>}
];
timezone_info(<<"FRUST">>) -> [
    {true, 25200, <<"Asia/Bishkek">>},
    {true, 21600, <<"Asia/Bishkek">>}
];
timezone_info(<<"FRUT">>) -> [
    {false, 21600, <<"Asia/Bishkek">>},
    {false, 18000, <<"Asia/Bishkek">>}
];
timezone_info(<<"GALT">>) -> [
    {false, -21600, <<"Pacific/Galapagos">>}
];
timezone_info(<<"GAMT">>) -> [
    {false, -32400, <<"Pacific/Gambier">>}
];
timezone_info(<<"GBGT">>) -> [
    {false, -13500, <<"America/Guyana">>}
];
timezone_info(<<"GEST">>) -> [
    {true, 14400, <<"Asia/Tbilisi">>},
    {true, 18000, <<"Asia/Tbilisi">>}
];
timezone_info(<<"GET">>) -> [
    {false, 14400, <<"Asia/Tbilisi">>},
    {false, 10800, <<"Asia/Tbilisi">>}
];
timezone_info(<<"GFT">>) -> [
    {false, -14400, <<"America/Cayenne">>},
    {false, -10800, <<"America/Cayenne">>}
];
timezone_info(<<"GHST">>) -> [
    {true, 1200, <<"Africa/Accra">>}
];
timezone_info(<<"GILT">>) -> [
    {false, 43200, <<"Pacific/Tarawa">>}
];
timezone_info(<<"GMT">>) -> [
    {false, 0, <<"Africa/Abidjan">>},
    {false, 0, <<"Africa/Accra">>},
    {false, 0, <<"Africa/Bamako">>},
    {false, 0, <<"Africa/Banjul">>},
    {false, 0, <<"Africa/Bissau">>},
    {false, 0, <<"Africa/Conakry">>},
    {false, 0, <<"Africa/Dakar">>},
    {false, 0, <<"Africa/Freetown">>},
    {false, 0, <<"Africa/Lome">>},
    {false, 0, <<"Africa/Malabo">>},
    {false, 0, <<"Africa/Monrovia">>},
    {false, 0, <<"Africa/Niamey">>},
    {false, 0, <<"Africa/Nouakchott">>},
    {false, 0, <<"Africa/Ouagadougou">>},
    {false, 0, <<"Africa/Porto-Novo">>},
    {false, 0, <<"Africa/Sao_Tome">>},
    {false, 0, <<"Africa/Timbuktu">>},
    {false, 0, <<"America/Danmarkshavn">>},
    {false, 0, <<"Atlantic/Reykjavik">>},
    {false, 0, <<"Atlantic/St_Helena">>},
    {false, 0, <<"Etc/GMT">>},
    {false, 0, <<"Etc/Greenwich">>},
    {false, 0, <<"Europe/Belfast">>},
    {false, 0, <<"Europe/Dublin">>},
    {false, 0, <<"Europe/Gibraltar">>},
    {false, 0, <<"Europe/Guernsey">>},
    {false, 0, <<"Europe/Isle_of_Man">>},
    {false, 0, <<"Europe/Jersey">>},
    {false, 0, <<"Europe/London">>},
    {false, 0, <<"GB">>}
];
timezone_info(<<"GST">>) -> [
    {false, 14400, <<"Asia/Dubai">>},
    {false, -7200, <<"Atlantic/South_Georgia">>},
    {false, 14400, <<"Asia/Bahrain">>},
    {false, 14400, <<"Asia/Muscat">>},
    {false, 14400, <<"Asia/Qatar">>},
    {false, 36000, <<"Pacific/Guam">>}
];
timezone_info(<<"GYT">>) -> [
    {false, -14400, <<"America/Guyana">>},
    {false, -10800, <<"America/Guyana">>},
    {false, -13500, <<"America/Guyana">>}
];
timezone_info(<<"HADT">>) -> [
    {true, -32400, <<"America/Adak">>},
    {true, -32400, <<"America/Atka">>}
];
timezone_info(<<"HAST">>) -> [
    {false, -36000, <<"America/Adak">>},
    {false, -36000, <<"America/Atka">>}
];
timezone_info(<<"HDT">>) -> [
    {true, -34200, <<"Pacific/Honolulu">>},
    {true, -34200, <<"HST">>},
    {true, -34200, <<"Pacific/Johnston">>}
];
timezone_info(<<"HKST">>) -> [
    {true, 32400, <<"Asia/Hong_Kong">>}
];
timezone_info(<<"HKT">>) -> [
    {false, 28800, <<"Asia/Hong_Kong">>}
];
timezone_info(<<"HMT">>) -> [
    {false, -19776, <<"America/Havana">>},
    {false, -6872, <<"Atlantic/Azores">>},
    {false, 21200, <<"Asia/Calcutta">>},
    {false, 21200, <<"Asia/Dacca">>},
    {false, 21200, <<"Asia/Dhaka">>},
    {false, 21200, <<"Asia/Kolkata">>},
    {false, 5989, <<"EET">>},
    {false, 5989, <<"Europe/Helsinki">>},
    {false, 5989, <<"Europe/Mariehamn">>}
];
timezone_info(<<"HOVST">>) -> [
    {true, 28800, <<"Asia/Hovd">>}
];
timezone_info(<<"HOVT">>) -> [
    {false, 25200, <<"Asia/Hovd">>},
    {false, 21600, <<"Asia/Hovd">>}
];
timezone_info(<<"HST">>) -> [
    {false, -36000, <<"Pacific/Honolulu">>},
    {false, -37800, <<"Pacific/Honolulu">>},
    {false, -36000, <<"HST">>},
    {false, -36000, <<"Pacific/Johnston">>},
    {false, -37800, <<"HST">>},
    {false, -37800, <<"Pacific/Johnston">>}
];
timezone_info(<<"ICT">>) -> [
    {false, 25200, <<"Asia/Bangkok">>},
    {false, 25200, <<"Asia/Ho_Chi_Minh">>},
    {false, 25200, <<"Asia/Phnom_Penh">>},
    {false, 25200, <<"Asia/Saigon">>},
    {false, 25200, <<"Asia/Vientiane">>},
    {false, 28800, <<"Asia/Ho_Chi_Minh">>},
    {false, 28800, <<"Asia/Phnom_Penh">>},
    {false, 28800, <<"Asia/Saigon">>},
    {false, 28800, <<"Asia/Vientiane">>}
];
timezone_info(<<"IDDT">>) -> [
    {true, 14400, <<"Asia/Jerusalem">>},
    {true, 14400, <<"Asia/Tel_Aviv">>}
];
timezone_info(<<"IDT">>) -> [
    {true, 10800, <<"Asia/Jerusalem">>},
    {true, 10800, <<"Asia/Gaza">>},
    {true, 10800, <<"Asia/Hebron">>},
    {true, 10800, <<"Asia/Tel_Aviv">>}
];
timezone_info(<<"IHST">>) -> [
    {true, 21600, <<"Asia/Colombo">>}
];
timezone_info(<<"IMT">>) -> [
    {false, 25035, <<"Asia/Irkutsk">>},
    {false, 7016, <<"Asia/Istanbul">>},
    {false, 7016, <<"Europe/Istanbul">>}
];
timezone_info(<<"IOT">>) -> [
    {false, 21600, <<"Indian/Chagos">>},
    {false, 18000, <<"Indian/Chagos">>}
];
timezone_info(<<"IRDT">>) -> [
    {true, 16200, <<"Asia/Tehran">>},
    {true, 18000, <<"Asia/Tehran">>}
];
timezone_info(<<"IRKST">>) -> [
    {true, 32400, <<"Asia/Irkutsk">>},
    {true, 28800, <<"Asia/Irkutsk">>}
];
timezone_info(<<"IRKT">>) -> [
    {false, 28800, <<"Asia/Irkutsk">>},
    {false, 25200, <<"Asia/Irkutsk">>},
    {false, 32400, <<"Asia/Irkutsk">>},
    {false, 28800, <<"Asia/Chita">>}
];
timezone_info(<<"IRST">>) -> [
    {false, 12600, <<"Asia/Tehran">>},
    {false, 14400, <<"Asia/Tehran">>}
];
timezone_info(<<"ISST">>) -> [
    {true, 0, <<"Atlantic/Reykjavik">>}
];
timezone_info(<<"IST">>) -> [
    {false, 7200, <<"Asia/Jerusalem">>},
    {false, -3600, <<"Atlantic/Reykjavik">>},
    {false, 19800, <<"Asia/Calcutta">>},
    {false, 19800, <<"Asia/Colombo">>},
    {false, 19800, <<"Asia/Dacca">>},
    {false, 19800, <<"Asia/Dhaka">>},
    {false, 19800, <<"Asia/Karachi">>},
    {false, 19800, <<"Asia/Kathmandu">>},
    {false, 19800, <<"Asia/Katmandu">>},
    {false, 19800, <<"Asia/Kolkata">>},
    {false, 19800, <<"Asia/Thimbu">>},
    {false, 19800, <<"Asia/Thimphu">>},
    {true, 2079, <<"Europe/Dublin">>},
    {true, 23400, <<"Asia/Calcutta">>},
    {true, 23400, <<"Asia/Colombo">>},
    {true, 23400, <<"Asia/Karachi">>},
    {true, 23400, <<"Asia/Kolkata">>},
    {false, 3600, <<"Europe/Dublin">>},
    {true, 3600, <<"Europe/Dublin">>},
    {false, 7200, <<"Asia/Gaza">>},
    {false, 7200, <<"Asia/Hebron">>},
    {false, 7200, <<"Asia/Tel_Aviv">>}
];
timezone_info(<<"JAVT">>) -> [
    {false, 26400, <<"Asia/Jakarta">>}
];
timezone_info(<<"JCST">>) -> [
    {false, 32400, <<"Asia/Pyongyang">>},
    {false, 32400, <<"Asia/Sakhalin">>},
    {false, 32400, <<"Asia/Seoul">>},
    {false, 32400, <<"Asia/Tokyo">>},
    {false, 32400, <<"ROK">>}
];
timezone_info(<<"JDT">>) -> [
    {true, 36000, <<"Asia/Tokyo">>}
];
timezone_info(<<"JMT">>) -> [
    {false, 8440, <<"Asia/Jerusalem">>},
    {false, 8440, <<"Asia/Tel_Aviv">>}
];
timezone_info(<<"JST">>) -> [
    {false, 32400, <<"Asia/Tokyo">>},
    {false, 32400, <<"Asia/Dili">>},
    {false, 32400, <<"Asia/Hong_Kong">>},
    {false, 32400, <<"Asia/Jakarta">>},
    {false, 32400, <<"Asia/Kuala_Lumpur">>},
    {false, 32400, <<"Asia/Kuching">>},
    {false, 32400, <<"Asia/Makassar">>},
    {false, 32400, <<"Asia/Manila">>},
    {false, 32400, <<"Asia/Pontianak">>},
    {false, 32400, <<"Asia/Pyongyang">>},
    {false, 32400, <<"Asia/Rangoon">>},
    {false, 32400, <<"Asia/Sakhalin">>},
    {false, 32400, <<"Asia/Seoul">>},
    {false, 32400, <<"Asia/Singapore">>},
    {false, 32400, <<"Asia/Taipei">>},
    {false, 32400, <<"Asia/Ujung_Pandang">>},
    {false, 32400, <<"Pacific/Nauru">>},
    {false, 32400, <<"ROC">>},
    {false, 32400, <<"ROK">>}
];
timezone_info(<<"JWST">>) -> [
    {false, 28800, <<"Asia/Taipei">>},
    {false, 28800, <<"ROC">>}
];
timezone_info(<<"KART">>) -> [
    {false, 18000, <<"Asia/Karachi">>}
];
timezone_info(<<"KDT">>) -> [
    {true, 36000, <<"Asia/Seoul">>},
    {true, 32400, <<"Asia/Seoul">>},
    {true, 32400, <<"ROK">>},
    {true, 36000, <<"ROK">>}
];
timezone_info(<<"KGST">>) -> [
    {true, 21600, <<"Asia/Bishkek">>}
];
timezone_info(<<"KGT">>) -> [
    {false, 18000, <<"Asia/Bishkek">>},
    {false, 21600, <<"Asia/Bishkek">>}
];
timezone_info(<<"KIZST">>) -> [
    {true, 21600, <<"Asia/Qyzylorda">>}
];
timezone_info(<<"KIZT">>) -> [
    {false, 21600, <<"Asia/Qyzylorda">>},
    {false, 14400, <<"Asia/Qyzylorda">>},
    {false, 18000, <<"Asia/Qyzylorda">>}
];
timezone_info(<<"KMT">>) -> [
    {false, 5736, <<"Europe/Vilnius">>},
    {false, -18431, <<"America/Cayman">>},
    {false, -18431, <<"America/Grand_Turk">>},
    {false, -18431, <<"America/Jamaica">>},
    {false, 7324, <<"Europe/Kiev">>}
];
timezone_info(<<"KOST">>) -> [
    {false, 43200, <<"Pacific/Kosrae">>},
    {false, 39600, <<"Pacific/Kosrae">>}
];
timezone_info(<<"KRAST">>) -> [
    {true, 28800, <<"Asia/Krasnoyarsk">>},
    {true, 25200, <<"Asia/Krasnoyarsk">>},
    {true, 25200, <<"Asia/Novokuznetsk">>},
    {true, 28800, <<"Asia/Novokuznetsk">>}
];
timezone_info(<<"KRAT">>) -> [
    {false, 25200, <<"Asia/Krasnoyarsk">>},
    {false, 21600, <<"Asia/Krasnoyarsk">>},
    {false, 28800, <<"Asia/Krasnoyarsk">>},
    {false, 21600, <<"Asia/Novokuznetsk">>},
    {false, 25200, <<"Asia/Novokuznetsk">>}
];
timezone_info(<<"KST">>) -> [
    {false, 28800, <<"Asia/Seoul">>},
    {false, 30600, <<"Asia/Seoul">>},
    {false, 32400, <<"Asia/Pyongyang">>},
    {false, 32400, <<"Asia/Seoul">>},
    {false, 28800, <<"Asia/Pyongyang">>},
    {false, 28800, <<"ROK">>},
    {false, 30600, <<"Asia/Pyongyang">>},
    {false, 30600, <<"ROK">>},
    {false, 32400, <<"ROK">>}
];
timezone_info(<<"KUYST">>) -> [
    {true, 18000, <<"Europe/Samara">>}
];
timezone_info(<<"KUYT">>) -> [
    {false, 14400, <<"Europe/Samara">>},
    {false, 10800, <<"Europe/Samara">>}
];
timezone_info(<<"KWAT">>) -> [
    {false, -43200, <<"Pacific/Kwajalein">>}
];
timezone_info(<<"LHDT">>) -> [
    {true, 39600, <<"Australia/LHI">>},
    {true, 39600, <<"Australia/Lord_Howe">>},
    {true, 41400, <<"Australia/LHI">>},
    {true, 41400, <<"Australia/Lord_Howe">>}
];
timezone_info(<<"LHST">>) -> [
    {false, 37800, <<"Australia/Lord_Howe">>},
    {false, 37800, <<"Australia/LHI">>}
];
timezone_info(<<"LINT">>) -> [
    {false, 50400, <<"Pacific/Kiritimati">>},
    {false, -36000, <<"Pacific/Kiritimati">>},
    {false, -38400, <<"Pacific/Kiritimati">>}
];
timezone_info(<<"LKT">>) -> [
    {false, 23400, <<"Asia/Colombo">>},
    {false, 21600, <<"Asia/Colombo">>}
];
timezone_info(<<"LRT">>) -> [
    {false, -2670, <<"Africa/Monrovia">>}
];
timezone_info(<<"LST">>) -> [
    {true, 9388, <<"Europe/Riga">>}
];
timezone_info(<<"MADMT">>) -> [
    {true, 3600, <<"Atlantic/Madeira">>}
];
timezone_info(<<"MADST">>) -> [
    {true, 0, <<"Atlantic/Madeira">>}
];
timezone_info(<<"MADT">>) -> [
    {false, -3600, <<"Atlantic/Madeira">>}
];
timezone_info(<<"MAGST">>) -> [
    {true, 43200, <<"Asia/Magadan">>},
    {true, 39600, <<"Asia/Magadan">>},
    {true, 39600, <<"Asia/Srednekolymsk">>},
    {true, 39600, <<"Asia/Ust-Nera">>},
    {true, 43200, <<"Asia/Srednekolymsk">>},
    {true, 43200, <<"Asia/Ust-Nera">>}
];
timezone_info(<<"MAGT">>) -> [
    {false, 36000, <<"Asia/Magadan">>},
    {false, 39600, <<"Asia/Magadan">>},
    {false, 43200, <<"Asia/Magadan">>},
    {false, 36000, <<"Asia/Srednekolymsk">>},
    {false, 36000, <<"Asia/Ust-Nera">>},
    {false, 39600, <<"Asia/Srednekolymsk">>},
    {false, 39600, <<"Asia/Ust-Nera">>},
    {false, 43200, <<"Asia/Srednekolymsk">>},
    {false, 43200, <<"Asia/Ust-Nera">>}
];
timezone_info(<<"MALST">>) -> [
    {true, 26400, <<"Asia/Singapore">>},
    {true, 26400, <<"Asia/Kuala_Lumpur">>}
];
timezone_info(<<"MALT">>) -> [
    {false, 27000, <<"Asia/Singapore">>},
    {false, 25200, <<"Asia/Singapore">>},
    {false, 26400, <<"Asia/Singapore">>},
    {false, 25200, <<"Asia/Kuala_Lumpur">>},
    {false, 26400, <<"Asia/Kuala_Lumpur">>},
    {false, 27000, <<"Asia/Kuala_Lumpur">>}
];
timezone_info(<<"MART">>) -> [
    {false, -34200, <<"Pacific/Marquesas">>}
];
timezone_info(<<"MAWT">>) -> [
    {false, 21600, <<"Antarctica/Mawson">>},
    {false, 18000, <<"Antarctica/Mawson">>}
];
timezone_info(<<"MDDT">>) -> [
    {true, -18000, <<"America/Cambridge_Bay">>},
    {true, -18000, <<"America/Yellowknife">>}
];
timezone_info(<<"MDST">>) -> [
    {true, 16279, <<"Europe/Moscow">>}
];
timezone_info(<<"MDT">>) -> [
    {true, -21600, <<"America/Denver">>},
    {true, -21600, <<"America/Bahia_Banderas">>},
    {true, -21600, <<"America/Boise">>},
    {true, -21600, <<"America/Cambridge_Bay">>},
    {true, -21600, <<"America/Chihuahua">>},
    {true, -21600, <<"America/Edmonton">>},
    {true, -21600, <<"America/Hermosillo">>},
    {true, -21600, <<"America/Inuvik">>},
    {true, -21600, <<"America/Mazatlan">>},
    {true, -21600, <<"America/North_Dakota/Beulah">>},
    {true, -21600, <<"America/North_Dakota/Center">>},
    {true, -21600, <<"America/North_Dakota/New_Salem">>},
    {true, -21600, <<"America/Ojinaga">>},
    {true, -21600, <<"America/Phoenix">>},
    {true, -21600, <<"America/Regina">>},
    {true, -21600, <<"America/Shiprock">>},
    {true, -21600, <<"America/Swift_Current">>},
    {true, -21600, <<"America/Yellowknife">>},
    {true, -21600, <<"Canada/East-Saskatchewan">>},
    {true, -21600, <<"Canada/Mountain">>},
    {true, -21600, <<"Canada/Saskatchewan">>},
    {true, -21600, <<"Mexico/BajaSur">>},
    {true, -21600, <<"MST">>}
];
timezone_info(<<"MEST">>) -> [
    {true, 7200, <<"MET">>}
];
timezone_info(<<"MET">>) -> [
    {false, 3600, <<"MET">>}
];
timezone_info(<<"MHT">>) -> [
    {false, 43200, <<"Pacific/Kwajalein">>},
    {false, 39600, <<"Pacific/Kwajalein">>},
    {false, 39600, <<"Pacific/Majuro">>},
    {false, 43200, <<"Pacific/Majuro">>}
];
timezone_info(<<"MIST">>) -> [
    {false, 39600, <<"Antarctica/Macquarie">>}
];
timezone_info(<<"MMT">>) -> [
    {false, 9017, <<"Europe/Moscow">>},
    {false, 9079, <<"Europe/Moscow">>},
    {false, -13484, <<"America/Montevideo">>},
    {false, -20712, <<"America/Managua">>},
    {false, -2588, <<"Africa/Monrovia">>},
    {false, 17640, <<"Indian/Maldives">>},
    {false, 19172, <<"Asia/Colombo">>},
    {false, 23400, <<"Asia/Rangoon">>},
    {false, 28656, <<"Asia/Makassar">>},
    {false, 28656, <<"Asia/Ujung_Pandang">>},
    {false, 6600, <<"Europe/Minsk">>}
];
timezone_info(<<"MOST">>) -> [
    {true, 32400, <<"Asia/Macao">>},
    {true, 32400, <<"Asia/Macau">>}
];
timezone_info(<<"MOT">>) -> [
    {false, 28800, <<"Asia/Macao">>},
    {false, 28800, <<"Asia/Macau">>}
];
timezone_info(<<"MPT">>) -> [
    {true, -21600, <<"America/Denver">>},
    {false, 36000, <<"Pacific/Saipan">>},
    {true, -21600, <<"America/Boise">>},
    {true, -21600, <<"America/Cambridge_Bay">>},
    {true, -21600, <<"America/Edmonton">>},
    {true, -21600, <<"America/North_Dakota/Beulah">>},
    {true, -21600, <<"America/North_Dakota/Center">>},
    {true, -21600, <<"America/North_Dakota/New_Salem">>},
    {true, -21600, <<"America/Regina">>},
    {true, -21600, <<"America/Shiprock">>},
    {true, -21600, <<"America/Swift_Current">>},
    {true, -21600, <<"America/Yellowknife">>},
    {true, -21600, <<"Canada/East-Saskatchewan">>},
    {true, -21600, <<"Canada/Mountain">>},
    {true, -21600, <<"Canada/Saskatchewan">>},
    {true, -21600, <<"MST">>},
    {false, 32400, <<"Pacific/Saipan">>}
];
timezone_info(<<"MSD">>) -> [
    {true, 14400, <<"Europe/Moscow">>},
    {true, 14400, <<"Europe/Chisinau">>},
    {true, 14400, <<"Europe/Kaliningrad">>},
    {true, 14400, <<"Europe/Kiev">>},
    {true, 14400, <<"Europe/Minsk">>},
    {true, 14400, <<"Europe/Riga">>},
    {true, 14400, <<"Europe/Samara">>},
    {true, 14400, <<"Europe/Simferopol">>},
    {true, 14400, <<"Europe/Tallinn">>},
    {true, 14400, <<"Europe/Tiraspol">>},
    {true, 14400, <<"Europe/Uzhgorod">>},
    {true, 14400, <<"Europe/Vilnius">>},
    {true, 14400, <<"Europe/Zaporozhye">>}
];
timezone_info(<<"MSK">>) -> [
    {false, 10800, <<"Europe/Moscow">>},
    {false, 14400, <<"Europe/Moscow">>},
    {false, 10800, <<"Europe/Chisinau">>},
    {false, 10800, <<"Europe/Kaliningrad">>},
    {false, 10800, <<"Europe/Kiev">>},
    {false, 10800, <<"Europe/Minsk">>},
    {false, 10800, <<"Europe/Riga">>},
    {false, 10800, <<"Europe/Samara">>},
    {false, 10800, <<"Europe/Simferopol">>},
    {false, 10800, <<"Europe/Tallinn">>},
    {false, 10800, <<"Europe/Tiraspol">>},
    {false, 10800, <<"Europe/Uzhgorod">>},
    {false, 10800, <<"Europe/Vilnius">>},
    {false, 10800, <<"Europe/Volgograd">>},
    {false, 10800, <<"Europe/Zaporozhye">>},
    {false, 14400, <<"Europe/Simferopol">>},
    {false, 14400, <<"Europe/Volgograd">>},
    {true, 14400, <<"Europe/Volgograd">>}
];
timezone_info(<<"MSM">>) -> [
    {true, 18000, <<"Europe/Moscow">>}
];
timezone_info(<<"MST">>) -> [
    {false, -25200, <<"America/Denver">>},
    {false, -25200, <<"America/Bahia_Banderas">>},
    {false, -25200, <<"America/Boise">>},
    {false, -25200, <<"America/Cambridge_Bay">>},
    {false, -25200, <<"America/Chihuahua">>},
    {false, -25200, <<"America/Creston">>},
    {false, -25200, <<"America/Dawson_Creek">>},
    {false, -25200, <<"America/Edmonton">>},
    {false, -25200, <<"America/Ensenada">>},
    {false, -25200, <<"America/Hermosillo">>},
    {false, -25200, <<"America/Inuvik">>},
    {false, -25200, <<"America/Mazatlan">>},
    {false, -25200, <<"America/Mexico_City">>},
    {false, -25200, <<"America/North_Dakota/Beulah">>},
    {false, -25200, <<"America/North_Dakota/Center">>},
    {false, -25200, <<"America/North_Dakota/New_Salem">>},
    {false, -25200, <<"America/Ojinaga">>},
    {false, -25200, <<"America/Phoenix">>},
    {false, -25200, <<"America/Regina">>},
    {false, -25200, <<"America/Santa_Isabel">>},
    {false, -25200, <<"America/Shiprock">>},
    {false, -25200, <<"America/Swift_Current">>},
    {false, -25200, <<"America/Tijuana">>},
    {false, -25200, <<"America/Yellowknife">>},
    {false, -25200, <<"Canada/East-Saskatchewan">>},
    {false, -25200, <<"Canada/Mountain">>},
    {false, -25200, <<"Canada/Saskatchewan">>},
    {false, -25200, <<"Mexico/BajaNorte">>},
    {false, -25200, <<"Mexico/BajaSur">>},
    {false, -25200, <<"Mexico/General">>},
    {false, -25200, <<"MST">>},
    {true, 12679, <<"Europe/Moscow">>}
];
timezone_info(<<"MUST">>) -> [
    {true, 18000, <<"Indian/Mauritius">>}
];
timezone_info(<<"MUT">>) -> [
    {false, 14400, <<"Indian/Mauritius">>}
];
timezone_info(<<"MVT">>) -> [
    {false, 18000, <<"Indian/Maldives">>}
];
timezone_info(<<"MWT">>) -> [
    {true, -21600, <<"America/Denver">>},
    {true, -21600, <<"America/Boise">>},
    {true, -21600, <<"America/Cambridge_Bay">>},
    {true, -21600, <<"America/Edmonton">>},
    {true, -21600, <<"America/North_Dakota/Beulah">>},
    {true, -21600, <<"America/North_Dakota/Center">>},
    {true, -21600, <<"America/North_Dakota/New_Salem">>},
    {true, -21600, <<"America/Phoenix">>},
    {true, -21600, <<"America/Regina">>},
    {true, -21600, <<"America/Shiprock">>},
    {true, -21600, <<"America/Swift_Current">>},
    {true, -21600, <<"America/Yellowknife">>},
    {true, -21600, <<"Canada/East-Saskatchewan">>},
    {true, -21600, <<"Canada/Mountain">>},
    {true, -21600, <<"Canada/Saskatchewan">>},
    {true, -21600, <<"MST">>}
];
timezone_info(<<"MYT">>) -> [
    {false, 28800, <<"Asia/Kuala_Lumpur">>},
    {false, 28800, <<"Asia/Kuching">>}
];
timezone_info(<<"NCST">>) -> [
    {true, 43200, <<"Pacific/Noumea">>}
];
timezone_info(<<"NCT">>) -> [
    {false, 39600, <<"Pacific/Noumea">>}
];
timezone_info(<<"NDDT">>) -> [
    {true, -5400, <<"America/St_Johns">>},
    {true, -5400, <<"Canada/Newfoundland">>}
];
timezone_info(<<"NDT">>) -> [
    {true, -9052, <<"America/St_Johns">>},
    {true, -9000, <<"America/St_Johns">>},
    {true, -36000, <<"Pacific/Midway">>},
    {true, -9000, <<"America/Goose_Bay">>},
    {true, -9000, <<"Canada/Newfoundland">>},
    {true, -9052, <<"America/Goose_Bay">>},
    {true, -9052, <<"Canada/Newfoundland">>}
];
timezone_info(<<"NEGT">>) -> [
    {false, -12600, <<"America/Paramaribo">>}
];
timezone_info(<<"NEST">>) -> [
    {true, 4800, <<"Europe/Amsterdam">>}
];
timezone_info(<<"NET">>) -> [
    {false, 1200, <<"Europe/Amsterdam">>}
];
timezone_info(<<"NFT">>) -> [
    {false, 41400, <<"Pacific/Norfolk">>}
];
timezone_info(<<"NMT">>) -> [
    {false, 20928, <<"Asia/Novokuznetsk">>},
    {false, 40320, <<"Pacific/Norfolk">>}
];
timezone_info(<<"NOVST">>) -> [
    {true, 25200, <<"Asia/Novosibirsk">>},
    {true, 28800, <<"Asia/Novosibirsk">>},
    {true, 25200, <<"Asia/Novokuznetsk">>}
];
timezone_info(<<"NOVT">>) -> [
    {false, 21600, <<"Asia/Novosibirsk">>},
    {false, 25200, <<"Asia/Novosibirsk">>},
    {false, 21600, <<"Asia/Novokuznetsk">>},
    {false, 25200, <<"Asia/Novokuznetsk">>}
];
timezone_info(<<"NPT">>) -> [
    {true, -9000, <<"America/St_Johns">>},
    {false, 20700, <<"Asia/Katmandu">>},
    {true, -36000, <<"America/Adak">>},
    {true, -36000, <<"America/Atka">>},
    {true, -36000, <<"America/Nome">>},
    {true, -9000, <<"America/Goose_Bay">>},
    {true, -9000, <<"Canada/Newfoundland">>},
    {false, 20700, <<"Asia/Kathmandu">>}
];
timezone_info(<<"NRT">>) -> [
    {false, 43200, <<"Pacific/Nauru">>},
    {false, 41400, <<"Pacific/Nauru">>}
];
timezone_info(<<"NST">>) -> [
    {false, -12600, <<"America/St_Johns">>},
    {false, -12652, <<"America/St_Johns">>},
    {true, 4772, <<"Europe/Amsterdam">>},
    {false, -12600, <<"America/Goose_Bay">>},
    {false, -12600, <<"Canada/Newfoundland">>},
    {false, -12652, <<"America/Goose_Bay">>},
    {false, -12652, <<"Canada/Newfoundland">>},
    {false, -39600, <<"America/Adak">>},
    {false, -39600, <<"America/Atka">>},
    {false, -39600, <<"America/Nome">>},
    {false, -39600, <<"Pacific/Midway">>},
    {false, -39600, <<"Pacific/Pago_Pago">>},
    {false, -39600, <<"Pacific/Samoa">>}
];
timezone_info(<<"NUT">>) -> [
    {false, -39600, <<"Pacific/Niue">>},
    {false, -40800, <<"Pacific/Niue">>},
    {false, -41400, <<"Pacific/Niue">>}
];
timezone_info(<<"NWT">>) -> [
    {true, -9000, <<"America/St_Johns">>},
    {true, -36000, <<"America/Adak">>},
    {true, -36000, <<"America/Atka">>},
    {true, -36000, <<"America/Nome">>},
    {true, -9000, <<"America/Goose_Bay">>},
    {true, -9000, <<"Canada/Newfoundland">>}
];
timezone_info(<<"NZDT">>) -> [
    {true, 46800, <<"Pacific/Auckland">>},
    {true, 46800, <<"Antarctica/McMurdo">>},
    {true, 46800, <<"Antarctica/South_Pole">>},
    {true, 46800, <<"NZ">>}
];
timezone_info(<<"NZMT">>) -> [
    {false, 41400, <<"Pacific/Auckland">>},
    {false, 41400, <<"Antarctica/McMurdo">>},
    {false, 41400, <<"Antarctica/South_Pole">>},
    {false, 41400, <<"NZ">>}
];
timezone_info(<<"NZST">>) -> [
    {false, 43200, <<"Pacific/Auckland">>},
    {true, 43200, <<"Pacific/Auckland">>},
    {true, 45000, <<"Pacific/Auckland">>},
    {false, 43200, <<"Antarctica/McMurdo">>},
    {false, 43200, <<"Antarctica/South_Pole">>},
    {false, 43200, <<"NZ">>},
    {true, 43200, <<"Antarctica/McMurdo">>},
    {true, 43200, <<"Antarctica/South_Pole">>},
    {true, 43200, <<"NZ">>},
    {true, 45000, <<"Antarctica/McMurdo">>},
    {true, 45000, <<"Antarctica/South_Pole">>},
    {true, 45000, <<"NZ">>}
];
timezone_info(<<"OMSST">>) -> [
    {true, 25200, <<"Asia/Omsk">>},
    {true, 21600, <<"Asia/Omsk">>}
];
timezone_info(<<"OMST">>) -> [
    {false, 21600, <<"Asia/Omsk">>},
    {false, 18000, <<"Asia/Omsk">>},
    {false, 25200, <<"Asia/Omsk">>}
];
timezone_info(<<"ORAST">>) -> [
    {true, 18000, <<"Asia/Oral">>}
];
timezone_info(<<"ORAT">>) -> [
    {false, 18000, <<"Asia/Oral">>},
    {false, 14400, <<"Asia/Oral">>}
];
timezone_info(<<"PDDT">>) -> [
    {true, -21600, <<"America/Inuvik">>}
];
timezone_info(<<"PDT">>) -> [
    {true, -25200, <<"America/Los_Angeles">>},
    {true, -25200, <<"America/Boise">>},
    {true, -25200, <<"America/Dawson">>},
    {true, -25200, <<"America/Dawson_Creek">>},
    {true, -25200, <<"America/Ensenada">>},
    {true, -25200, <<"America/Juneau">>},
    {true, -25200, <<"America/Metlakatla">>},
    {true, -25200, <<"America/Santa_Isabel">>},
    {true, -25200, <<"America/Sitka">>},
    {true, -25200, <<"America/Tijuana">>},
    {true, -25200, <<"America/Vancouver">>},
    {true, -25200, <<"America/Whitehorse">>},
    {true, -25200, <<"Canada/Pacific">>},
    {true, -25200, <<"Canada/Yukon">>},
    {true, -25200, <<"Mexico/BajaNorte">>}
];
timezone_info(<<"PEST">>) -> [
    {true, -14400, <<"America/Lima">>}
];
timezone_info(<<"PETST">>) -> [
    {true, 46800, <<"Asia/Kamchatka">>},
    {true, 43200, <<"Asia/Kamchatka">>}
];
timezone_info(<<"PETT">>) -> [
    {false, 43200, <<"Asia/Kamchatka">>},
    {false, 39600, <<"Asia/Kamchatka">>}
];
timezone_info(<<"PET">>) -> [
    {false, -18000, <<"America/Lima">>}
];
timezone_info(<<"PGT">>) -> [
    {false, 36000, <<"Pacific/Port_Moresby">>}
];
timezone_info(<<"PHOT">>) -> [
    {false, 46800, <<"Pacific/Enderbury">>},
    {false, -39600, <<"Pacific/Enderbury">>},
    {false, -43200, <<"Pacific/Enderbury">>}
];
timezone_info(<<"PHST">>) -> [
    {true, 32400, <<"Asia/Manila">>}
];
timezone_info(<<"PHT">>) -> [
    {false, 28800, <<"Asia/Manila">>}
];
timezone_info(<<"PKST">>) -> [
    {true, 21600, <<"Asia/Karachi">>}
];
timezone_info(<<"PKT">>) -> [
    {false, 18000, <<"Asia/Karachi">>}
];
timezone_info(<<"PMDT">>) -> [
    {true, -7200, <<"America/Miquelon">>}
];
timezone_info(<<"PMST">>) -> [
    {false, -10800, <<"America/Miquelon">>}
];
timezone_info(<<"PMT">>) -> [
    {false, -13236, <<"America/Paramaribo">>},
    {false, -13252, <<"America/Paramaribo">>},
    {false, 36000, <<"Antarctica/DumontDUrville">>},
    {false, 26240, <<"Asia/Pontianak">>},
    {false, 561, <<"Africa/Algiers">>},
    {false, 561, <<"Africa/Tunis">>},
    {false, 561, <<"Europe/Monaco">>},
    {false, 561, <<"Europe/Paris">>},
    {false, 561, <<"WET">>}
];
timezone_info(<<"PNT">>) -> [
    {false, -30600, <<"Pacific/Pitcairn">>}
];
timezone_info(<<"PONT">>) -> [
    {false, 39600, <<"Pacific/Pohnpei">>},
    {false, 39600, <<"Pacific/Ponape">>}
];
timezone_info(<<"PPMT">>) -> [
    {false, -17340, <<"America/Port-au-Prince">>}
];
timezone_info(<<"PPT">>) -> [
    {true, -25200, <<"America/Los_Angeles">>},
    {true, -25200, <<"America/Dawson_Creek">>},
    {true, -25200, <<"America/Ensenada">>},
    {true, -25200, <<"America/Juneau">>},
    {true, -25200, <<"America/Metlakatla">>},
    {true, -25200, <<"America/Santa_Isabel">>},
    {true, -25200, <<"America/Sitka">>},
    {true, -25200, <<"America/Tijuana">>},
    {true, -25200, <<"America/Vancouver">>},
    {true, -25200, <<"Canada/Pacific">>},
    {true, -25200, <<"Mexico/BajaNorte">>}
];
timezone_info(<<"PST">>) -> [
    {false, -28800, <<"America/Los_Angeles">>},
    {false, -28800, <<"America/Bahia_Banderas">>},
    {false, -28800, <<"America/Boise">>},
    {false, -28800, <<"America/Creston">>},
    {false, -28800, <<"America/Dawson">>},
    {false, -28800, <<"America/Dawson_Creek">>},
    {false, -28800, <<"America/Ensenada">>},
    {false, -28800, <<"America/Hermosillo">>},
    {false, -28800, <<"America/Inuvik">>},
    {false, -28800, <<"America/Juneau">>},
    {false, -28800, <<"America/Mazatlan">>},
    {false, -28800, <<"America/Metlakatla">>},
    {false, -28800, <<"America/Santa_Isabel">>},
    {false, -28800, <<"America/Sitka">>},
    {false, -28800, <<"America/Tijuana">>},
    {false, -28800, <<"America/Vancouver">>},
    {false, -28800, <<"America/Whitehorse">>},
    {false, -28800, <<"Canada/Pacific">>},
    {false, -28800, <<"Canada/Yukon">>},
    {false, -28800, <<"Mexico/BajaNorte">>},
    {false, -28800, <<"Mexico/BajaSur">>},
    {false, -28800, <<"Pacific/Pitcairn">>}
];
timezone_info(<<"PWT">>) -> [
    {true, -25200, <<"America/Los_Angeles">>},
    {true, -25200, <<"America/Dawson_Creek">>},
    {true, -25200, <<"America/Ensenada">>},
    {true, -25200, <<"America/Juneau">>},
    {true, -25200, <<"America/Metlakatla">>},
    {true, -25200, <<"America/Santa_Isabel">>},
    {true, -25200, <<"America/Sitka">>},
    {true, -25200, <<"America/Tijuana">>},
    {true, -25200, <<"America/Vancouver">>},
    {true, -25200, <<"Canada/Pacific">>},
    {true, -25200, <<"Mexico/BajaNorte">>},
    {false, 32400, <<"Pacific/Palau">>}
];
timezone_info(<<"PYST">>) -> [
    {true, -10800, <<"America/Asuncion">>}
];
timezone_info(<<"PYT">>) -> [
    {false, -14400, <<"America/Asuncion">>},
    {false, -10800, <<"America/Asuncion">>}
];
timezone_info(<<"QMT">>) -> [
    {false, -18840, <<"America/Guayaquil">>}
];
timezone_info(<<"QYZST">>) -> [
    {true, 25200, <<"Asia/Qyzylorda">>}
];
timezone_info(<<"QYZT">>) -> [
    {false, 21600, <<"Asia/Qyzylorda">>},
    {false, 18000, <<"Asia/Qyzylorda">>}
];
timezone_info(<<"RET">>) -> [
    {false, 14400, <<"Indian/Reunion">>}
];
timezone_info(<<"RMT">>) -> [
    {false, 5788, <<"Europe/Riga">>},
    {false, -5268, <<"Atlantic/Reykjavik">>},
    {false, 23080, <<"Asia/Rangoon">>}
];
timezone_info(<<"ROTT">>) -> [
    {false, -10800, <<"Antarctica/Rothera">>}
];
timezone_info(<<"SAKST">>) -> [
    {true, 39600, <<"Asia/Sakhalin">>},
    {true, 43200, <<"Asia/Sakhalin">>}
];
timezone_info(<<"SAKT">>) -> [
    {false, 36000, <<"Asia/Sakhalin">>},
    {false, 39600, <<"Asia/Sakhalin">>}
];
timezone_info(<<"SAMST">>) -> [
    {true, 21600, <<"Asia/Samarkand">>},
    {true, 14400, <<"Europe/Samara">>},
    {true, 18000, <<"Europe/Samara">>}
];
timezone_info(<<"SAMT">>) -> [
    {false, 18000, <<"Asia/Samarkand">>},
    {false, 14400, <<"Asia/Samarkand">>},
    {false, 10800, <<"Europe/Samara">>},
    {false, 14400, <<"Europe/Samara">>}
];
timezone_info(<<"SAST">>) -> [
    {false, 7200, <<"Africa/Johannesburg">>},
    {true, 10800, <<"Africa/Johannesburg">>},
    {false, 5400, <<"Africa/Johannesburg">>},
    {true, 10800, <<"Africa/Maseru">>},
    {true, 10800, <<"Africa/Windhoek">>},
    {false, 5400, <<"Africa/Gaborone">>},
    {false, 7200, <<"Africa/Maseru">>},
    {false, 7200, <<"Africa/Mbabane">>},
    {false, 7200, <<"Africa/Windhoek">>}
];
timezone_info(<<"SBT">>) -> [
    {false, 39600, <<"Pacific/Guadalcanal">>}
];
timezone_info(<<"SCT">>) -> [
    {false, 14400, <<"Indian/Mahe">>}
];
timezone_info(<<"SDMT">>) -> [
    {false, -16800, <<"America/Santo_Domingo">>}
];
timezone_info(<<"SDT">>) -> [
    {true, -36000, <<"Pacific/Apia">>}
];
timezone_info(<<"SGT">>) -> [
    {false, 28800, <<"Asia/Singapore">>},
    {false, 27000, <<"Asia/Singapore">>}
];
timezone_info(<<"SHEST">>) -> [
    {true, 21600, <<"Asia/Aqtau">>}
];
timezone_info(<<"SHET">>) -> [
    {false, 21600, <<"Asia/Aqtau">>},
    {false, 18000, <<"Asia/Aqtau">>}
];
timezone_info(<<"SJMT">>) -> [
    {false, -20173, <<"America/Costa_Rica">>}
];
timezone_info(<<"SMT">>) -> [
    {false, 25580, <<"Asia/Saigon">>},
    {false, -13884, <<"Atlantic/Stanley">>},
    {false, -16966, <<"America/Santiago">>},
    {false, -16966, <<"Chile/Continental">>},
    {false, 24925, <<"Asia/Kuala_Lumpur">>},
    {false, 24925, <<"Asia/Singapore">>},
    {false, 25580, <<"Asia/Ho_Chi_Minh">>},
    {false, 25580, <<"Asia/Phnom_Penh">>},
    {false, 25580, <<"Asia/Vientiane">>},
    {false, 8160, <<"Europe/Simferopol">>}
];
timezone_info(<<"SRET">>) -> [
    {false, 39600, <<"Asia/Srednekolymsk">>}
];
timezone_info(<<"SRT">>) -> [
    {false, -10800, <<"America/Paramaribo">>},
    {false, -12600, <<"America/Paramaribo">>}
];
timezone_info(<<"SST">>) -> [
    {false, -39600, <<"Pacific/Samoa">>},
    {false, -39600, <<"Pacific/Apia">>},
    {false, -39600, <<"Pacific/Midway">>},
    {false, -39600, <<"Pacific/Pago_Pago">>}
];
timezone_info(<<"STAT">>) -> [
    {false, 10800, <<"Europe/Volgograd">>},
    {false, 14400, <<"Europe/Volgograd">>}
];
timezone_info(<<"SVEST">>) -> [
    {true, 21600, <<"Asia/Yekaterinburg">>},
    {true, 18000, <<"Asia/Yekaterinburg">>}
];
timezone_info(<<"SVET">>) -> [
    {false, 18000, <<"Asia/Yekaterinburg">>},
    {false, 14400, <<"Asia/Yekaterinburg">>}
];
timezone_info(<<"SWAT">>) -> [
    {false, 5400, <<"Africa/Windhoek">>}
];
timezone_info(<<"SYOT">>) -> [
    {false, 10800, <<"Antarctica/Syowa">>}
];
timezone_info(<<"TAHT">>) -> [
    {false, -36000, <<"Pacific/Tahiti">>}
];
timezone_info(<<"TASST">>) -> [
    {true, 25200, <<"Asia/Samarkand">>},
    {true, 21600, <<"Asia/Tashkent">>},
    {true, 25200, <<"Asia/Tashkent">>}
];
timezone_info(<<"TAST">>) -> [
    {false, 21600, <<"Asia/Samarkand">>},
    {false, 18000, <<"Asia/Tashkent">>},
    {false, 21600, <<"Asia/Tashkent">>}
];
timezone_info(<<"TBIST">>) -> [
    {true, 18000, <<"Asia/Tbilisi">>},
    {true, 14400, <<"Asia/Tbilisi">>}
];
timezone_info(<<"TBIT">>) -> [
    {false, 14400, <<"Asia/Tbilisi">>},
    {false, 10800, <<"Asia/Tbilisi">>}
];
timezone_info(<<"TBMT">>) -> [
    {false, 10746, <<"Asia/Tbilisi">>}
];
timezone_info(<<"TFT">>) -> [
    {false, 18000, <<"Indian/Kerguelen">>}
];
timezone_info(<<"TJT">>) -> [
    {false, 18000, <<"Asia/Dushanbe">>}
];
timezone_info(<<"TKT">>) -> [
    {false, -39600, <<"Pacific/Fakaofo">>},
    {false, 46800, <<"Pacific/Fakaofo">>}
];
timezone_info(<<"TLT">>) -> [
    {false, 32400, <<"Asia/Dili">>},
    {false, 28800, <<"Asia/Dili">>}
];
timezone_info(<<"TMT">>) -> [
    {false, 12344, <<"Asia/Tehran">>},
    {false, 5940, <<"Europe/Tallinn">>},
    {false, 14400, <<"Asia/Ashgabat">>},
    {false, 14400, <<"Asia/Ashkhabad">>},
    {false, 18000, <<"Asia/Ashgabat">>},
    {false, 18000, <<"Asia/Ashkhabad">>}
];
timezone_info(<<"TOST">>) -> [
    {true, 50400, <<"Pacific/Tongatapu">>}
];
timezone_info(<<"TOT">>) -> [
    {false, 46800, <<"Pacific/Tongatapu">>},
    {false, 44400, <<"Pacific/Tongatapu">>}
];
timezone_info(<<"TRST">>) -> [
    {true, 14400, <<"Europe/Istanbul">>},
    {true, 14400, <<"Asia/Istanbul">>}
];
timezone_info(<<"TRT">>) -> [
    {false, 10800, <<"Europe/Istanbul">>},
    {false, 10800, <<"Asia/Istanbul">>}
];
timezone_info(<<"TSAT">>) -> [
    {false, 10800, <<"Europe/Volgograd">>}
];
timezone_info(<<"TVT">>) -> [
    {false, 43200, <<"Pacific/Funafuti">>}
];
timezone_info(<<"UCT">>) -> [
    {false, 0, <<"Etc/UCT">>},
    {false, 0, <<"UCT">>}
];
timezone_info(<<"ULAST">>) -> [
    {true, 32400, <<"Asia/Ulaanbaatar">>},
    {true, 32400, <<"Asia/Ulan_Bator">>}
];
timezone_info(<<"ULAT">>) -> [
    {false, 28800, <<"Asia/Ulaanbaatar">>},
    {false, 25200, <<"Asia/Ulaanbaatar">>},
    {false, 25200, <<"Asia/Choibalsan">>},
    {false, 25200, <<"Asia/Ulan_Bator">>},
    {false, 28800, <<"Asia/Choibalsan">>},
    {false, 28800, <<"Asia/Ulan_Bator">>}
];
timezone_info(<<"URAST">>) -> [
    {true, 21600, <<"Asia/Oral">>},
    {true, 18000, <<"Asia/Oral">>}
];
timezone_info(<<"URAT">>) -> [
    {false, 21600, <<"Asia/Oral">>},
    {false, 14400, <<"Asia/Oral">>},
    {false, 18000, <<"Asia/Oral">>}
];
timezone_info(<<"UTC">>) -> [
    {false, 0, <<"Antarctica/Troll">>},
    {false, 0, <<"Etc/Universal">>},
    {false, 0, <<"Etc/UTC">>},
    {false, 0, <<"Etc/Zulu">>},
    {false, 0, <<"GMT">>},
    {false, 0, <<"UTC">>},
    {false, 0, <<"UTC">>}
];
timezone_info(<<"UYHST">>) -> [
    {true, -9000, <<"America/Montevideo">>},
    {true, -10800, <<"America/Montevideo">>}
];
timezone_info(<<"UYST">>) -> [
    {true, -7200, <<"America/Montevideo">>}
];
timezone_info(<<"UYT">>) -> [
    {false, -10800, <<"America/Montevideo">>},
    {false, -12600, <<"America/Montevideo">>}
];
timezone_info(<<"UZST">>) -> [
    {true, 21600, <<"Asia/Samarkand">>},
    {true, 21600, <<"Asia/Tashkent">>}
];
timezone_info(<<"UZT">>) -> [
    {false, 18000, <<"Asia/Samarkand">>},
    {false, 18000, <<"Asia/Tashkent">>}
];
timezone_info(<<"VET">>) -> [
    {false, -16200, <<"America/Caracas">>},
    {false, -14400, <<"America/Caracas">>}
];
timezone_info(<<"VLAST">>) -> [
    {true, 39600, <<"Asia/Vladivostok">>},
    {true, 36000, <<"Asia/Vladivostok">>},
    {true, 39600, <<"Asia/Khandyga">>}
];
timezone_info(<<"VLAT">>) -> [
    {false, 36000, <<"Asia/Vladivostok">>},
    {false, 32400, <<"Asia/Vladivostok">>},
    {false, 39600, <<"Asia/Vladivostok">>},
    {false, 36000, <<"Asia/Khandyga">>},
    {false, 36000, <<"Asia/Ust-Nera">>},
    {false, 39600, <<"Asia/Khandyga">>},
    {false, 39600, <<"Asia/Ust-Nera">>}
];
timezone_info(<<"VOLST">>) -> [
    {true, 14400, <<"Europe/Volgograd">>},
    {true, 18000, <<"Europe/Volgograd">>}
];
timezone_info(<<"VOLT">>) -> [
    {false, 10800, <<"Europe/Volgograd">>},
    {false, 14400, <<"Europe/Volgograd">>}
];
timezone_info(<<"VOST">>) -> [
    {false, 21600, <<"Antarctica/Vostok">>}
];
timezone_info(<<"VUST">>) -> [
    {true, 43200, <<"Pacific/Efate">>}
];
timezone_info(<<"VUT">>) -> [
    {false, 39600, <<"Pacific/Efate">>}
];
timezone_info(<<"WAKT">>) -> [
    {false, 43200, <<"Pacific/Wake">>}
];
timezone_info(<<"WARST">>) -> [
    {true, -10800, <<"America/Mendoza">>},
    {true, -10800, <<"America/Argentina/Jujuy">>},
    {true, -10800, <<"America/Argentina/Mendoza">>},
    {true, -10800, <<"America/Argentina/San_Luis">>},
    {true, -10800, <<"America/Jujuy">>}
];
timezone_info(<<"WART">>) -> [
    {false, -14400, <<"America/Mendoza">>},
    {false, -14400, <<"America/Argentina/Catamarca">>},
    {false, -14400, <<"America/Argentina/ComodRivadavia">>},
    {false, -14400, <<"America/Argentina/Cordoba">>},
    {false, -14400, <<"America/Argentina/Jujuy">>},
    {false, -14400, <<"America/Argentina/La_Rioja">>},
    {false, -14400, <<"America/Argentina/Mendoza">>},
    {false, -14400, <<"America/Argentina/Rio_Gallegos">>},
    {false, -14400, <<"America/Argentina/Salta">>},
    {false, -14400, <<"America/Argentina/San_Juan">>},
    {false, -14400, <<"America/Argentina/San_Luis">>},
    {false, -14400, <<"America/Argentina/Tucuman">>},
    {false, -14400, <<"America/Argentina/Ushuaia">>},
    {false, -14400, <<"America/Catamarca">>},
    {false, -14400, <<"America/Cordoba">>},
    {false, -14400, <<"America/Jujuy">>},
    {false, -14400, <<"America/Rosario">>}
];
timezone_info(<<"WAST">>) -> [
    {true, 7200, <<"Africa/Windhoek">>},
    {true, 7200, <<"Africa/Ndjamena">>}
];
timezone_info(<<"WAT">>) -> [
    {false, 3600, <<"Africa/Brazzaville">>},
    {false, -3600, <<"Africa/Bissau">>},
    {false, -3600, <<"Africa/El_Aaiun">>},
    {false, -3600, <<"Africa/Niamey">>},
    {false, 3600, <<"Africa/Bangui">>},
    {false, 3600, <<"Africa/Douala">>},
    {false, 3600, <<"Africa/Kinshasa">>},
    {false, 3600, <<"Africa/Lagos">>},
    {false, 3600, <<"Africa/Libreville">>},
    {false, 3600, <<"Africa/Luanda">>},
    {false, 3600, <<"Africa/Malabo">>},
    {false, 3600, <<"Africa/Ndjamena">>},
    {false, 3600, <<"Africa/Niamey">>},
    {false, 3600, <<"Africa/Porto-Novo">>},
    {false, 3600, <<"Africa/Windhoek">>}
];
timezone_info(<<"WEMT">>) -> [
    {true, 7200, <<"Europe/Lisbon">>},
    {true, 7200, <<"Europe/Madrid">>},
    {true, 7200, <<"Europe/Monaco">>},
    {true, 7200, <<"Europe/Paris">>},
    {true, 7200, <<"WET">>}
];
timezone_info(<<"WEST">>) -> [
    {true, 3600, <<"Europe/Paris">>},
    {true, 7200, <<"Europe/Luxembourg">>},
    {true, 3600, <<"Africa/Algiers">>},
    {true, 3600, <<"Africa/Casablanca">>},
    {true, 3600, <<"Africa/Ceuta">>},
    {true, 3600, <<"Africa/El_Aaiun">>},
    {true, 3600, <<"Atlantic/Canary">>},
    {true, 3600, <<"Atlantic/Faeroe">>},
    {true, 3600, <<"Atlantic/Faroe">>},
    {true, 3600, <<"Atlantic/Madeira">>},
    {true, 3600, <<"Europe/Brussels">>},
    {true, 3600, <<"Europe/Lisbon">>},
    {true, 3600, <<"Europe/Luxembourg">>},
    {true, 3600, <<"Europe/Madrid">>},
    {true, 3600, <<"Europe/Monaco">>},
    {true, 3600, <<"WET">>}
];
timezone_info(<<"WET">>) -> [
    {false, 0, <<"Europe/Paris">>},
    {false, 3600, <<"Europe/Luxembourg">>},
    {false, 0, <<"Africa/Algiers">>},
    {false, 0, <<"Africa/Casablanca">>},
    {false, 0, <<"Africa/Ceuta">>},
    {false, 0, <<"Africa/El_Aaiun">>},
    {false, 0, <<"Atlantic/Azores">>},
    {false, 0, <<"Atlantic/Canary">>},
    {false, 0, <<"Atlantic/Faeroe">>},
    {false, 0, <<"Atlantic/Faroe">>},
    {false, 0, <<"Atlantic/Madeira">>},
    {false, 0, <<"Europe/Andorra">>},
    {false, 0, <<"Europe/Brussels">>},
    {false, 0, <<"Europe/Lisbon">>},
    {false, 0, <<"Europe/Luxembourg">>},
    {false, 0, <<"Europe/Madrid">>},
    {false, 0, <<"Europe/Monaco">>},
    {false, 0, <<"WET">>}
];
timezone_info(<<"WFT">>) -> [
    {false, 43200, <<"Pacific/Wallis">>}
];
timezone_info(<<"WGST">>) -> [
    {true, -7200, <<"America/Godthab">>},
    {true, -7200, <<"America/Danmarkshavn">>}
];
timezone_info(<<"WGT">>) -> [
    {false, -10800, <<"America/Godthab">>},
    {false, -10800, <<"America/Danmarkshavn">>}
];
timezone_info(<<"WIB">>) -> [
    {false, 25200, <<"Asia/Jakarta">>},
    {false, 25200, <<"Asia/Pontianak">>},
    {false, 27000, <<"Asia/Jakarta">>},
    {false, 27000, <<"Asia/Pontianak">>},
    {false, 28800, <<"Asia/Jakarta">>},
    {false, 28800, <<"Asia/Pontianak">>}
];
timezone_info(<<"WITA">>) -> [
    {false, 28800, <<"Asia/Dili">>},
    {false, 28800, <<"Asia/Makassar">>},
    {false, 28800, <<"Asia/Pontianak">>},
    {false, 28800, <<"Asia/Ujung_Pandang">>}
];
timezone_info(<<"WIT">>) -> [
    {false, 32400, <<"Asia/Jayapura">>}
];
timezone_info(<<"WMT">>) -> [
    {false, 5040, <<"Europe/Vilnius">>},
    {false, 5040, <<"Europe/Warsaw">>}
];
timezone_info(<<"WSDT">>) -> [
    {true, 50400, <<"Pacific/Apia">>}
];
timezone_info(<<"WSST">>) -> [
    {false, -41400, <<"Pacific/Apia">>},
    {false, 46800, <<"Pacific/Apia">>}
];
timezone_info(<<"XJT">>) -> [
    {false, 21600, <<"Asia/Kashgar">>},
    {false, 21600, <<"Asia/Urumqi">>}
];
timezone_info(<<"YAKST">>) -> [
    {true, 36000, <<"Asia/Yakutsk">>},
    {true, 32400, <<"Asia/Yakutsk">>},
    {true, 32400, <<"Asia/Chita">>},
    {true, 32400, <<"Asia/Khandyga">>},
    {true, 36000, <<"Asia/Chita">>},
    {true, 36000, <<"Asia/Khandyga">>}
];
timezone_info(<<"YAKT">>) -> [
    {false, 32400, <<"Asia/Yakutsk">>},
    {false, 28800, <<"Asia/Yakutsk">>},
    {false, 36000, <<"Asia/Yakutsk">>},
    {false, 28800, <<"Asia/Chita">>},
    {false, 28800, <<"Asia/Khandyga">>},
    {false, 28800, <<"Asia/Ust-Nera">>},
    {false, 32400, <<"Asia/Chita">>},
    {false, 32400, <<"Asia/Khandyga">>},
    {false, 32400, <<"Asia/Ust-Nera">>},
    {false, 36000, <<"Asia/Chita">>},
    {false, 36000, <<"Asia/Khandyga">>}
];
timezone_info(<<"YDDT">>) -> [
    {true, -25200, <<"America/Dawson">>},
    {true, -25200, <<"America/Whitehorse">>},
    {true, -25200, <<"Canada/Yukon">>}
];
timezone_info(<<"YDT">>) -> [
    {true, -28800, <<"America/Dawson">>},
    {true, -28800, <<"America/Juneau">>},
    {true, -28800, <<"America/Whitehorse">>},
    {true, -28800, <<"America/Yakutat">>},
    {true, -28800, <<"Canada/Yukon">>}
];
timezone_info(<<"YEKST">>) -> [
    {true, 21600, <<"Asia/Yekaterinburg">>}
];
timezone_info(<<"YEKT">>) -> [
    {false, 18000, <<"Asia/Yekaterinburg">>},
    {false, 21600, <<"Asia/Yekaterinburg">>}
];
timezone_info(<<"YERST">>) -> [
    {true, 18000, <<"Asia/Yerevan">>},
    {true, 14400, <<"Asia/Yerevan">>}
];
timezone_info(<<"YERT">>) -> [
    {false, 14400, <<"Asia/Yerevan">>},
    {false, 10800, <<"Asia/Yerevan">>}
];
timezone_info(<<"YPT">>) -> [
    {true, -28800, <<"America/Dawson">>},
    {true, -28800, <<"America/Whitehorse">>},
    {true, -28800, <<"America/Yakutat">>},
    {true, -28800, <<"Canada/Yukon">>}
];
timezone_info(<<"YST">>) -> [
    {false, -32400, <<"America/Anchorage">>},
    {false, -32400, <<"America/Dawson">>},
    {false, -32400, <<"America/Juneau">>},
    {false, -32400, <<"America/Nome">>},
    {false, -32400, <<"America/Sitka">>},
    {false, -32400, <<"America/Whitehorse">>},
    {false, -32400, <<"America/Yakutat">>},
    {false, -32400, <<"Canada/Yukon">>}
];
timezone_info(<<"YWT">>) -> [
    {true, -28800, <<"America/Dawson">>},
    {true, -28800, <<"America/Whitehorse">>},
    {true, -28800, <<"America/Yakutat">>},
    {true, -28800, <<"Canada/Yukon">>}
];
timezone_info(<<"A">>) -> [
    {false, 3600, undefined}
];
timezone_info(<<"B">>) -> [
    {false, 7200, undefined}
];
timezone_info(<<"C">>) -> [
    {false, 10800, undefined}
];
timezone_info(<<"D">>) -> [
    {false, 14400, undefined}
];
timezone_info(<<"E">>) -> [
    {false, 18000, undefined}
];
timezone_info(<<"F">>) -> [
    {false, 21600, undefined}
];
timezone_info(<<"G">>) -> [
    {false, 25200, undefined}
];
timezone_info(<<"H">>) -> [
    {false, 28800, undefined}
];
timezone_info(<<"I">>) -> [
    {false, 32400, undefined}
];
timezone_info(<<"K">>) -> [
    {false, 36000, undefined}
];
timezone_info(<<"L">>) -> [
    {false, 39600, undefined}
];
timezone_info(<<"M">>) -> [
    {false, 43200, undefined}
];
timezone_info(<<"N">>) -> [
    {false, -3600, undefined}
];
timezone_info(<<"O">>) -> [
    {false, -7200, undefined}
];
timezone_info(<<"P">>) -> [
    {false, -10800, undefined}
];
timezone_info(<<"Q">>) -> [
    {false, -14400, undefined}
];
timezone_info(<<"R">>) -> [
    {false, -18000, undefined}
];
timezone_info(<<"S">>) -> [
    {false, -21600, undefined}
];
timezone_info(<<"T">>) -> [
    {false, -25200, undefined}
];
timezone_info(<<"U">>) -> [
    {false, -28800, undefined}
];
timezone_info(<<"V">>) -> [
    {false, -32400, undefined}
];
timezone_info(<<"W">>) -> [
    {false, -36000, undefined}
];
timezone_info(<<"X">>) -> [
    {false, -39600, undefined}
];
timezone_info(<<"Y">>) -> [
    {false, -43200, undefined}
];
timezone_info(<<"ZZZ">>) -> [
    {false, 0, <<"Antarctica/Davis">>},
    {false, 0, <<"America/Cambridge_Bay">>},
    {false, 0, <<"America/Inuvik">>},
    {false, 0, <<"America/Iqaluit">>},
    {false, 0, <<"America/Pangnirtung">>},
    {false, 0, <<"America/Rankin_Inlet">>},
    {false, 0, <<"America/Resolute">>},
    {false, 0, <<"America/Yellowknife">>},
    {false, 0, <<"Antarctica/Casey">>},
    {false, 0, <<"Antarctica/DumontDUrville">>},
    {false, 0, <<"Antarctica/Macquarie">>},
    {false, 0, <<"Antarctica/Mawson">>},
    {false, 0, <<"Antarctica/Palmer">>},
    {false, 0, <<"Antarctica/Rothera">>},
    {false, 0, <<"Antarctica/Syowa">>},
    {false, 0, <<"Antarctica/Troll">>},
    {false, 0, <<"Antarctica/Vostok">>},
    {false, 0, <<"Indian/Kerguelen">>}
];
timezone_info(<<"Z">>) -> [
    {false, 0, undefined}
].
