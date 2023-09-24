*------------------------------------------------------

*This script is for the data sets (GC / M / C) from November, 2015. Collections ran for 3 weeks, Includes tweets with 0 retweets
Syntax below is for testing retweet rates with negative binomial regression;

*--------------------------------------------------------;

*First load all data--------------------;

%web_drop_table(WORK.a)
FILENAME REFFILE '/folders/myfolders/main/MEC_SASpreproc_All_updated.csv';
PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.a;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=WORK.a; RUN;
%web_open_table(WORK.a);


*GUN CONTROL SET----------------;
*---------------------------------;
*--------------------------------;
*-----------------------------------;

DATA g_all_center;
   SET a;
   IF topic = "G";
RUN;

*request means to grand-mean center;
PROC MEANS DATA=g_all_center;
  VAR acount_unq mcount_unq shared shared_pos shared_neg poscount_unq negcount_unq followers ideology ;
RUN;

*centering;
data g_all_center;
set g_all_center;
followers_C = followers - 39055.97;
acount_unq_C = acount_unq - 0.5431665;
mcount_unq_C = mcount_unq - 0.8177047;
shared_C = shared - 0.2582138;
shared_pos_C = shared_pos - 0.0627764;
shared_neg_C = shared_neg - 0.1486961;
poscount_unq_C = poscount_unq - 0.2925569;
negcount_unq_c = negcount_unq - 0.1933298;
run;

*fx code variables;
data g_all_center;
set g_all_center;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (crosstalk =1) then crosstalk_fx = .5;
if (crosstalk =0) then crosstalk_fx = -.5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

data g_all_center;
set g_all_center;
if shared = 0 then shared_bin = -.5;
if shared > 0 then shared_bin = .5;
run;


*---------
*shared analysis, where shared = "moral-emotional words", acount_unq = unique emotion words, mcount_unq = unique moral words;
*---------;
proc genmod data=g_all_center;
model count = shared_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_C followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*binary analysis;
proc genmod data=g_all_center;
model count = shared_bin
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_bin followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*main models;
proc genmod data=g_all_center;
model count = acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;

*valence analysis: test whether driven by pos vs neg valence;
proc genmod data=g_all_center;
model count = negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
model count = poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
*to determine which drives enter as competing predictors;
proc genmod data=gg_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=g_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;

*Run GEE for sesitivitiy analyses based on non-independent observations----------------------;
*run w/ exchangable correlation structure;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*run with independent correlation structure;
proc genmod data=g_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=g_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;



*SAME-SEX MARRIAGE SET----------------;
*---------------------------------;
*--------------------------------;
*-----------------------------------;

DATA m_all_center;
   SET a;
   IF topic = "M";
RUN;

*request means to grand-mean center;
PROC MEANS DATA=m_all_center;
  VAR acount_unq mcount_unq shared shared_pos shared_neg poscount_unq negcount_unq followers ideology;
RUN;

*centering;
data m_all_center;
set m_all_center;
followers_C = followers - 55772.79;
acount_unq_C = acount_unq - 0.5536820;
mcount_unq_C = mcount_unq - 0.6060908;
shared_C = shared - 0.1545423;
ideo_sq = sqrt(ideo_SD);
shared_pos_C = shared_pos - 0.0451824;
shared_neg_C = shared_neg - 0.0891948;
poscount_unq_C = poscount_unq - 0.3810392;
negcount_unq_c = negcount_unq - 0.1344116;
run;

*fx code variables;
data m_all_center;
set m_all_center;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (crosstalk =1) then crosstalk_fx = .5;
if (crosstalk =0) then crosstalk_fx = -.5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

data m_all_center;
set m_all_center;
if shared = 0 then shared_bin = -.5;
if shared > 0 then shared_bin = .5;
run;


*---------
*shared analysis, where shared = "moral-emotional words", acount_unq = unique emotion words, mcount_unq = unique moral words;
*---------;
proc genmod data=m_all_center;
model count = shared_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_C followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*binary analysis;
proc genmod data=m_all_center;
model count = shared_bin
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_bin followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*main model;
proc genmod data=m_all_center;
model count = acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;

*valence analysis: test whether driven by pos vs neg valence;
proc genmod data=m_all_center;
model count = negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
model count = poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
*to determine which drives enter as competing predictors;
proc genmod data=gm_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=m_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;


*Run GEE for sesitivitiy analyses based on non-independent observations----------------------;
*run w/ exchangable correlation structure;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;

*run with indepenndent correlation structure;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=m_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;




*CLIMATE CHANGE SET----------------;
*---------------------------------;
*--------------------------------;
*-----------------------------------;

DATA c_all_center;
   SET a;
   IF topic = "C";
RUN;

*request means to grand-mean center;
PROC MEANS DATA=c_all_center;
  VAR acount_unq mcount_unq shared shared_pos shared_neg poscount_unq negcount_unq ideology  followers;
RUN;

*centering;
data c_all_center;
set c_all_center;
followers_C = followers - 39055.97;
acount_unq_C = acount_unq - 0.7334216;
mcount_unq_C = mcount_unq - 0.2301527;
shared_C = shared - 0.2271766;
shared_pos_C = shared_pos - 0.0590071;
shared_neg_C = shared_neg - 0.1256432;
poscount_unq_C = poscount_unq - 0.4456841;
negcount_unq_c = negcount_unq - 0.2205835;
run;

*fx code variables;
data c_all_center;
set c_all_center;
if (media = -1) then media_fx = -.5;
if (media = 1) then media_fx = .5;
if (url = -1) then url_fx = -.5;
if (url = 1) then url_fx = .5;
if (crosstalk =1) then crosstalk_fx = .5;
if (crosstalk =0) then crosstalk_fx = -.5;
if (user_verified =1) then verified = .5;
if (user_verified =0) then verified = -.5;
run;

data c_all_center;
set c_all_center;
if shared = 0 then shared_bin = -.5;
if shared > 0 then shared_bin = .5;
run;


*---------
*shared analysis, where shared = "moral-emotional words", acount_unq = unique emotion words, mcount_unq = unique moral words;
*---------;
proc genmod data=c_all_center;
model count = shared_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_C followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*binary analysis;
proc genmod data=c_all_center;
model count = shared_bin
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = shared_bin followers_C url_Fx media_Fx verified
/ dist=nb type3;
run;

*main models;
proc genmod data=c_all_center;
model count = acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C
/ dist=nb type3;
run;


*valence analysis: test whether driven by pos vs neg valence;
proc genmod data=c_all_center;
model count = negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified negcount_unq_C mcount_unq_C shared_neg_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
model count = poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C mcount_unq_C shared_pos_C
/ dist=nb type3;
run;
*to determine which drives enter as competing predictors;
proc genmod data=gc_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;
proc genmod data=c_all_center;
class url_Fx(REF=FIRST) media_Fx(REF=FIRST) verified(REF=FIRST);
model count = followers_C url_Fx media_Fx verified poscount_unq_C negcount_unq_C mcount_unq_C shared_neg_C shared_pos_C
/ dist=nb type3;
run;


*Run GEE for sesitivitiy analyses based on non-independent observations----------------------;
*run w/ exchangable correlation structure;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=exch covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*run with indepenndent correlation structure;
proc genmod data=c_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count =  acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;
*ods exclude GEENcorr GEENcov GEERCorr GEERcov GEEWcorr;
proc genmod data=c_all_center;
   class src_id url_FX(ref=first) media_fx(ref=first)  verified(REF=FIRST) ;
   model  count = followers_C url_Fx media_Fx verified acount_unq_C mcount_unq_C shared_C /  dist=negbin;
   repeated  subject=src_id / type=ind covb corrw;
    *EFFECTPLOT FIT (PLOTBY = ingroup x = shared_C);
run;





