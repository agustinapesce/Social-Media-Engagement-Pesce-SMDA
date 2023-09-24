

*Gun Control data set analyses-----------------------
---------------------------
----------------------------;

*import .csv;
%web_drop_table(WORK.g);
FILENAME REFFILE '/folders/myfolders/MLM_ReallyFinal/gee_modDist_G.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.g;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.g; RUN;
%web_open_table(WORK.g);

*request means;
PROC MEANS DATA=g;
  VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data g;
set g;
acount_unq_C = acount_unq - 0.5417330;
mcount_unq_C = mcount_unq - 0.8292905;
shared_C = shared - 0.2590452;
followers_C = followers - 29584.05;
run;

*fx code variables;
data g;
set g;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*-----------------------------------;
*-----------------------------------;
*Ingroup outgroup analysis;
*-----------------------------------;
*-----------------------------------;
*no covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g;
   class twid_first ingroup(REF=last) url_fx(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*3way int partyxMExingroup;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*-----------------------------------;
*-----------------------------------;
*Conservative liberal analysis;
*-----------------------------------;
*-----------------------------------;
*no covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g;
   class twid_first rep_rt(REF=last) url(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

*w. covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g;
   class twid_first rep_rt(REF=last) url(ref=first) media(ref=first) verified(ref=first) ;
   model  count = url media verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;



*verified users removed--------------------------;
*------------------------------;
*---------------------------------------;
data gv;
 SET g;
 if user_verified ^= 1;
 run;
 
PROC MEANS DATA=gv;
  VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data gv;
set gv;
acount_unq_C = acount_unq - 0.5439621;
mcount_unq_C = mcount_unq - 0.8270485;
shared_C = shared - 0.2589915;
followers_C = followers - 6164.88;
run;

*fx code variables;
data gv;
set gv;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;


*verified users removed analyses;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=gv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first)  ;
   model  count = url_fx media_fx  followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=gv;
   class twid_first rep_rt(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=gv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------10% mods removed---------------------;
*--------------------;
*-------------------;

data g10;
 SET g;
 if mod_dist > .10;
 run;
 
 PROC MEANS DATA=g10;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data g10;
set g10;
acount_unq_C = acount_unq - 0.5432233;
mcount_unq_C = mcount_unq -0.8286993;
shared_C = shared - 0.2630302;
followers_C = followers - 30249.27;
run;

*fx code variables;
data g10;
set g10;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g10;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------20% mods removed---------------------;
*--------------------;
*-------------------;

data g20;
 SET g;
 if mod_dist > .20;
 run;
 
 PROC MEANS DATA=g20;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data g20;
set g20;
acount_unq_C = acount_unq - 0.5464158;
mcount_unq_C = mcount_unq - 0.8312205;
shared_C = shared - 0.2663009;
followers_C = followers - 24246.00;
run;

*fx code variables;
data g20;
set g20;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g20;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;



*Marriage data set analyses-----------------------;
---------------------------;
----------------------------;
----------------------------;
----------------------------;
---------------------------;
*import;
%web_drop_table(WORK.m);
FILENAME REFFILE '/folders/myfolders/MLM_ReallyFinal/gee_modDist_M.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.m;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.m; RUN;
%web_open_table(WORK.m);

*request means;
PROC MEANS DATA=m;
  VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data m;
set m;
acount_unq_C = acount_unq - 0.5471720;
mcount_unq_C = mcount_unq - 0.6164797;
shared_C = shared - 0.1564384;
followers_C = followers - 37398.25;
run;

*fx code variables;
data m;
set m;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_FX = -.5;
if (url = 1) then url_FX = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*-----------------------------------;
*-----------------------------------;
*Ingroup outgroup analysis;
*-----------------------------------;
*-----------------------------------;


ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m;
   class twid_first ingroup(REF=last) url_fx(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*3way int partyxMExingroup;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;


*-----------------------------------;
*-----------------------------------;
*Conservative liberal analysis;
*-----------------------------------;
*-----------------------------------;
*no covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m;
   class twid_first rep_rt(REF=last) url(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

*w. covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m;
   class twid_first rep_rt(REF=last) url(ref=first) media(ref=first) verified(ref=first) ;
   model  count = url media verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;


*verified users removed--------------;
data mv;
 SET m;
 if user_verified ^= 1;
 run;
 
PROC MEANS DATA=mv;
VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data mv;
set mv;
acount_unq_C = acount_unq - 0.5520862;
mcount_unq_C = mcount_unq - 0.6162777;
shared_C = shared - 0.1581012;
followers_C = followers - 10133.39;
run;

*fx code variables;
data mv;
set mv;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*verified users removed analyses;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=mv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first)  ;
   model  count = url_fx media_fx  followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=mv;
   class twid_first rep_rt(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=mv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------10% mods removed---------------------;
*--------------------;
*-------------------;

data m10;
 SET m;
 if mod_dist > .10;
 run;
 
PROC MEANS DATA=m10;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data m10;
set m10;
acount_unq_C = acount_unq - 0.5522110;
mcount_unq_C = mcount_unq -0.6088099;
shared_C = shared - 0.1558486;
followers_C = followers - 31371.83;
run;

*fx code variables;
data m10;
set m10;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m10;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------20% mods removed---------------------;
*--------------------;
*-------------------;

data m20;
 SET m;
 if mod_dist > .20;
 run;
 
PROC MEANS DATA=m20;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data m20;
set m20;
acount_unq_C = acount_unq - 0.5582758;
mcount_unq_C = mcount_unq - 0.5911884;
shared_C = shared - 0.1575137;
followers_C = followers - 31235.65;
run;

*fx code variables;
data m20;
set m20;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m20;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;




*climate-----------------------;
---------------------------;
----------------------------;
----------------------------;
----------------------------;
---------------------------;
%web_drop_table(WORK.IMPORT);
FILENAME REFFILE '/folders/myfolders/MLM_ReallyFinal/gee_modDist_C.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.C;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.C; RUN;
%web_open_table(WORK.C);


PROC MEANS DATA=c;
  VAR acount_unq mcount_unq shared followers;
RUN;


*centering;
data c;
set c;
acount_unq_C = acount_unq - 0.7457150;
mcount_unq_C = mcount_unq - 0.2340034;
shared_C = shared - 0.2340376;
followers_C = followers - 34062.64;
run;

*fx code variables;
data c;
set c;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*-----------------------------------;
*-----------------------------------;
*Ingroup outgroup analysis;
*-----------------------------------;
*-----------------------------------;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c;
   class twid_first ingroup(REF=first) url(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*3way int partyxMExingroup;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;


*-----------------------------------;
*-----------------------------------;
*Conservative liberal analysis;
*-----------------------------------;
*-----------------------------------;
*no covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c;
   class twid_first rep_rt(REF=last) url(ref=first) ;
   model  count = acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt  /  dist=negbin CL;
   repeated  subject=twid_first / type=exch covb corrw;
   * EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

*w. covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c;
   class twid_first rep_rt(REF=last) url(ref=first) media(ref=first) verified(ref=first) ;
   model  count = url media verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

*verified users removed--------------;

data cv;
 SET c;
 if user_verified ^= 1;
 run;
 
PROC MEANS DATA=cv;
VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data cv;
set cv;
acount_unq_C = acount_unq - 0.7508098;
mcount_unq_C = mcount_unq - 0.2344038;
shared_C = shared - 0.2354499;
followers_C = followers - 7471.74;
run;

*fx code variables;
data cv;
set cv;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*verified users removed analyses;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=cv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first)  ;
   model  count = url_fx media_fx  followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=cv;
   class twid_first rep_rt(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C rep_rt shared_C*rep_rt acount_unq_C*rep_rt mcount_unq_C*rep_rt /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = rep_rt x = shared_C);
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=cv;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------10% mods removed---------------------;
*--------------------;
*-------------------;

data c10;
 SET c;
 if mod_dist > .10;
 run;
 
PROC MEANS DATA=c10;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data c10;
set c10;
acount_unq_C = acount_unq - 0.7584979;
mcount_unq_C = mcount_unq -0.2357916;
shared_C = shared - 0.2390648;
followers_C = followers - 31956.21;
run;

*fx code variables;
data c10;
set c10;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c10;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*------20% mods removed---------------------;
*--------------------;
*-------------------;

data c20;
 SET c;
 if mod_dist > .20;
 run;
 
PROC MEANS DATA=c20;
  VAR acount_unq mcount_unq shared followers mod_dist;
RUN;

*centering;
data c20;
set c20;
acount_unq_C = acount_unq - 0.7736174;
mcount_unq_C = mcount_unq - 0.2286573;
shared_C = shared - 0.2461840;
followers_C = followers - 31456.59;
run;

*fx code variables;
data c20;
set c20;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

*w/covs;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c20;
   class twid_first ingroup(REF=last) url_fx(ref=first) media_fx(ref=first) verified(ref=first) ;
   model  count = url_fx media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;



*all sets combined-------------------------------;
%web_drop_table(WORK.IMPORT);
FILENAME REFFILE '/folders/myfolders/MLM_ReallyFinal/gee_modDist_All.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.all;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.all; RUN;
%web_open_table(WORK.all);

PROC MEANS DATA=all;
  VAR acount_unq mcount_unq shared followers;
RUN;

*centering;
data all;
set all;
acount_unq_C = acount_unq - 0.6941663;
mcount_unq_C = mcount_unq - 0.3656628;
shared_C = shared - 0.2306380;
followers_C = followers - 33664.49;
run;

*fx code variables;
data all;
set all;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=all;
   class twid_first ingroup(REF=last) url_FX(ref=first) media_fx(ref=first) verified(ref=first) rep_src(ref=last) ;
   model  count = url_FX media_fx verified followers_C acount_unq_C mcount_unq_C shared_C ingroup rep_src
   shared_C*ingroup acount_unq_C*ingroup mcount_unq_C*ingroup
   shared_C*rep_src acount_unq_C*rep_src mcount_unq_C*rep_src ingroup*rep_src
   shared_C*ingroup*rep_src acount_unq_C*ingroup*rep_src mcount_unq_C*ingroup*rep_src
   /  dist=negbin;
   repeated  subject=twid_first / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
