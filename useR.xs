#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#include "ppport.h"

#include <stdio.h>
#include <stdlib.h>
#include <Rembedded.h>
#include <Rmath.h>
#include <Rinterface.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <R_ext/Arith.h>
#include <Rdefines.h>

typedef struct VECTOR_SEXPREC RVec;

//End R session
IV end_R() {
  Rf_endEmbeddedR(0);
  return EXIT_SUCCESS;
}

//Eval R command
RVec * eval_R(char * cmd) {
  SEXP cmdSexp, cmdexpr, ans = R_NilValue;
  ParseStatus status;
  int i;

  PROTECT(cmdSexp = allocVector(STRSXP, 1));
  SET_STRING_ELT(cmdSexp, 0, mkChar(cmd));
  cmdexpr = PROTECT(R_ParseVector(cmdSexp, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    UNPROTECT(2);
    error("invalie cmd %s", cmd);
  }
  for(i=0; i<length(cmdexpr); i++) {
    ans = eval(VECTOR_ELT(cmdexpr, i), R_GlobalEnv);
  }
  UNPROTECT(2);
  
  return (RVec*)ans;
}

//Return the data type of R object
const char * gettype(RVec * robj) {
  SEXPTYPE type = TYPEOF((SEXP)robj);
  switch(type) {
  case NILSXP:
    return "NILSXP";
  case SYMSXP:
    return "SYMSXP";
  case LISTSXP:
    return "LISTSXP";
  case CLOSXP:
    return "CLOSXP";
  case ENVSXP:
    return "ENVSXP";
  case PROMSXP:
    return "PROMSXP";
  case LANGSXP:
    return "LANGSXP";
  case SPECIALSXP:
    return "SPECIALSXP";
  case BUILTINSXP:
    return "BUILTINSXP";
  case CHARSXP:
    return "CHARSXP";
  case LGLSXP:
    return "LGLSXP";
  case INTSXP:
    return "INTSXP";
  case REALSXP:
    return "REALSXP";
  case CPLXSXP:
    return "CPLXSXP";
  case STRSXP:
    return "STRSXP";
  case DOTSXP:
    return "DOTSXP";
  case ANYSXP:
    return "ANYSXP";
  case VECSXP:
    return "VECSXP";
  case EXPRSXP:
    return "EXPRSXP";
  case BCODESXP:
    return "BCODESXP";
  case EXTPTRSXP:
    return "EXTPTRSXP";
  case WEAKREFSXP:
    return "WEAKREFSXP";
  case RAWSXP:
    return "RAWSXP";
  case S4SXP:
    return "S4SXP";
  case FUNSXP:
    return "FUNSXP";
  default:
    return "Unknown";
  }
}

#define GET_VEC_NAME(x,y) CHAR(STRING_ELT(GET_NAMES(x), y)); //Equal to names(obj)
#define PUT_RV_2HV(x,y,z) hv_store(x, y, strlen(y), newRV_noinc(z), 0); //copy one sv z, make a rv and then store it into hv x with key named as y

//Fetch integer vector
AV * fetchINT(RVec * robj) {
  AV * arr = newAV();
  int len, i;

  len = LENGTH((SEXP)robj);
  for(i=0;i<len;i++) {
    av_push(arr, newSViv((INTEGER((SEXP)robj))[i]));
  }
  return arr;
}

//Fetch real vector
AV * fetchREAL(RVec * robj) {
  AV * arr = newAV();
  int len, i;

  len = LENGTH((SEXP)robj);
  for(i=0;i<len;i++) {
    av_push(arr, newSVnv((REAL((SEXP)robj))[i]));
  }
  return arr;
}

//Fetch string vector
AV * fetchSTR(RVec * robj) {
  AV * arr = newAV();
  int len, i;

  len = LENGTH((SEXP)robj);
  for(i=0;i<len;i++) {
    av_push(arr, newSVpv(CHAR(STRING_ELT((SEXP)robj, i)), 0));
  }
  return arr;
}

//Fetch object(intsxp, realsxp, strsxp, vecsxp) value. Return a hash storing ref of av.
HV * fetchvalue(RVec * robj) {
  HV * res = newHV();
  AV * arr;
  int len, i;
  RVec * v;
  const char * name;

  switch(TYPEOF((SEXP)robj)) {
    case INTSXP:
      arr = fetchINT(robj);
      name = "int";
      PUT_RV_2HV(res, name, (SV*)arr);
      break;
    case REALSXP:
      arr = fetchREAL(robj);
      name = "real";
      PUT_RV_2HV(res, name, (SV*)arr);
      break;
    case STRSXP:
      arr = fetchSTR(robj);
      name = "str";
      PUT_RV_2HV(res, name, (SV*)arr);
      break;
    case VECSXP:
      len = LENGTH((SEXP)robj);
      for(i=0;i<len;i++) {
        name = GET_VEC_NAME((SEXP)robj, i);
	v = (RVec*)VECTOR_ELT((SEXP)robj, i);
	switch(TYPEOF((SEXP)v)) {
	  case INTSXP:
	    arr = fetchINT(v);
            PUT_RV_2HV(res, name, (SV*)arr);
	    break;
	  case REALSXP:
	    arr = fetchREAL(v);
	    PUT_RV_2HV(res, name, (SV*)arr);
	    break;
	  case STRSXP:
	    arr = fetchSTR(v);
	    PUT_RV_2HV(res, name, (SV*)arr);
	    break;
	  case VECSXP:
	    PUT_RV_2HV(res, name, (SV*)fetchvalue(v));
	    break;
	  default:
	    hv_store(res, name, strlen(name), &PL_sv_undef, 0);
	}
      }
      break;
    default:
      hv_store(res, "null", 4, &PL_sv_undef, 0);
  }

  return res;
}

RVec * setINT(AV * intarr) {
  RVec * intvec;
  int i;
  SV ** mem;

  PROTECT((SEXP)(intvec = (RVec*)allocVector(INTSXP, av_len(intarr)+1)));
  for(i=0;i<av_len(intarr)+1;i++) {
    mem = av_fetch(intarr, i, 0);
    if(SvTYPE(*mem)==SVt_PVIV || SvTYPE(*mem)==SVt_IV || SvTYPE(*mem)==SVt_PV) {
      INTEGER((SEXP)intvec)[i] = SvIV(*mem);
      
    }
    else if(SvTYPE(*mem)==SVt_NULL) {
      INTEGER((SEXP)intvec)[i] = NA_INTEGER;
    }
    else {
      croak("Data No.%d is not integer number", i+1);
    }
  }
  UNPROTECT(1);

  return intvec;
}

RVec * setREAL(AV * realarr) {
  RVec * realvec;
  int i;
  SV ** mem;

  PROTECT((SEXP)(realvec = (RVec*)allocVector(REALSXP, av_len(realarr)+1)));
  for(i=0;i<av_len(realarr)+1;i++) {
    mem = av_fetch(realarr, i, 0);
    if(SvTYPE(*mem)==SVt_PVNV || SvTYPE(*mem)==SVt_NV || SvTYPE(*mem)==SVt_PV) {
      REAL((SEXP)realvec)[i] = SvNV(*mem);
    }
    else if(SvTYPE(*mem)==SVt_NULL) {
      REAL((SEXP)realvec)[i] = NA_REAL;
    }
    else {
      croak("Data No.%d is not real number", i+1);
    }
  }
  UNPROTECT(1);

  return realvec;
}

RVec * setSTR(AV * strarr) {
  RVec * strvec;
  int i;
  SV ** mem;

  PROTECT((SEXP)(strvec = (RVec*)allocVector(STRSXP, av_len(strarr)+1)));
  for(i=0;i<av_len(strarr)+1;i++) {
    mem = av_fetch(strarr, i, 0);
    if(SvTYPE(*mem)==SVt_PV || SvTYPE(*mem) == SVt_PVIV || SVt_PVNV) {
      SET_STRING_ELT((SEXP)strvec, i, mkChar(SvPV_nolen(*mem)));
      
    }
    else if(SvTYPE(*mem)==SVt_NULL){
      SET_STRING_ELT((SEXP)strvec, i, NA_STRING);
    }
    else {
      croak("Data No.%d is not string", i+1);
    }
  }
  UNPROTECT(1);
  
  return strvec;
}

MODULE = Statistics::useR		PACKAGE = Statistics::useR

IV
init_R(optc=0,opts=NULL)
  int optc;
  char ** opts;
 PREINIT:
  char * defargv[] = {"REmbedded", "--quiet", "--vanilla"};
 CODE:
  if(opts==NULL){opts = defargv; optc = sizeof(defargv)/sizeof(char*);}
  RETVAL = Rf_initEmbeddedR(optc, opts);
 OUTPUT:
  RETVAL

RVec * eval_R(cmd)
  char * cmd;
  
IV
end_R()

MODULE = Statistics::useR		PACKAGE = Statistics::RData

const char *
gettype(robj)
  RVec * robj;

IV
getlen(robj)
  RVec * robj;
 CODE:
  RETVAL = LENGTH((SEXP)robj);
 OUTPUT:
  RETVAL

AV *
getnames(robj)
  RVec * robj;
 PREINIT:
  RVec * names;
  AV * nameav;
  int i;
 CODE:
  names = (RVec*)GET_NAMES((SEXP)robj);
  nameav = (AV*)sv_2mortal((SV*)newAV());
  for(i=0;i<LENGTH((SEXP)names);i++) {
    av_push(nameav, newSVpv((CHAR(STRING_ELT((SEXP)names,i))), 0));
  }
  RETVAL = nameav;
 OUTPUT:
  RETVAL

HV *
getdimnames(robj)
  RVec * robj;
 PREINIT:
  HV * namehv;
  AV * nameav;
  RVec * dimnames;
  RVec * names;
  int i,j;
  char name[2];
 CODE:
  namehv = (HV*)sv_2mortal((SV*)newHV());
  dimnames = (RVec*)GET_DIMNAMES((SEXP)robj);
  if(TYPEOF((SEXP)dimnames)!=VECSXP) {
   namehv=(HV*)&PL_sv_undef;
  }
  else {
    for(i=0;i<LENGTH((SEXP)dimnames);i++) {
      nameav = newAV();
      names = (RVec*)VECTOR_ELT((SEXP)dimnames,i);
      for(j=0;j<LENGTH((SEXP)names);j++) {
        av_push(nameav, newSVpv((CHAR(STRING_ELT((SEXP)names,j))), 0));
      }
    sprintf(name, "%c", i+48);
    hv_store(namehv, name, strlen(name), newRV_noinc((SV*)nameav), 0);
    }
  }
  RETVAL = namehv;
 OUTPUT:
  RETVAL


HV *
getvalue(robj)
  RVec * robj;
 CODE:
  RETVAL = (HV*)sv_2mortal((SV*)fetchvalue(robj));
 OUTPUT:
  RETVAL

RVec *
setvalue(datahv, typehv, keyno)
  HV * datahv;
  HV * typehv; //Contains 'int', 'real', 'str'
  IV keyno;
 PREINIT:
  RVec * res;
  RVec * names;
  AV * keys;
  AV * colvalue;
  int i,j,k;
  char * key;
 CODE:
  if(SvROK(ST(0)) && SvTYPE(SvRV(ST(0)))==SVt_PVHV) {
    datahv = (HV*)SvRV(ST(0));
  }
  else {
    croak("First argument is not hash ref");
  }
  typehv = (HV*)SvRV(ST(1));
  keyno = SvIV(ST(2)); 
  PROTECT((SEXP)(res = (RVec*)allocVector(VECSXP, keyno)));
  PROTECT((SEXP)(names = (RVec*)allocVector(STRSXP, keyno)));
  keys = (AV*)(SvRV(*(hv_fetch(typehv, "str", 3, 0))));
  for(i=0;i<=av_len(keys);i++) {
    key = SvPV_nolen(*(av_fetch(keys, i, 0)));
    SET_STRING_ELT((SEXP)names, i, mkChar(key));
    colvalue = (AV*)(SvRV(*(hv_fetch(datahv, key, strlen(key),0))));
    SET_VECTOR_ELT((SEXP)res, i, (SEXP)setSTR(colvalue));
  }
  i = av_len(keys);
  keys = (AV*)(SvRV(*(hv_fetch(typehv, "int", 3, 0))));
  for(j=0;j<=av_len(keys);j++) {
    key = SvPV_nolen(*(av_fetch(keys, j, 0)));
    SET_STRING_ELT((SEXP)names, (i+j+1), mkChar(key));
    colvalue = (AV*)(SvRV(*(hv_fetch(datahv, key, strlen(key),0))));
    SET_VECTOR_ELT((SEXP)res, (i+j+1), (SEXP)setINT(colvalue));
  }
  j = av_len(keys);
  keys = (AV*)(SvRV(*(hv_fetch(typehv, "real", 4, 0))));
  for(k=0;k<=av_len(keys);k++) {
    key = SvPV_nolen(*(av_fetch(keys, k, 0)));
    SET_STRING_ELT((SEXP)names, (i+j+k+2), mkChar(key));
    colvalue = (AV*)(SvRV(*(hv_fetch(datahv, key, strlen(key),0))));
    SET_VECTOR_ELT((SEXP)res, (i+j+k+2), (SEXP)setREAL(colvalue));
  }
  SET_NAMES((SEXP)res, (SEXP)names);
  UNPROTECT(2);
  RETVAL = res;
 OUTPUT:
  RETVAL

void
insvar(var, varname)
  RVec * var;
  char * varname;
 CODE:
  defineVar(install(varname), (SEXP)var, R_GlobalEnv);
  
