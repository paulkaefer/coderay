module CodeRay
module Scanners

  class SAS < Scanner
  
    register_for :sas
    
    file_extension 'sas'

    # List all token kinds that are not considered to be running code
    KINDS_NOT_LOC = [
      :comment
    ]  # :nodoc:

    # See the WordList documentation.
    #CONSTANTS = %w( true false null )
    #IDENT_KIND = WordList.new(:key).add(CONSTANTS, :value)

    #ESCAPE = / [bfnrt\\"\/] /x
    #UNICODE_ESCAPE = / u[a-fA-F0-9]{4} /x
    
    module Words # :nodoc:
    
        CONSTANT = %w[_n_ _null_]
    
        MACROBOUND = %w[%macro %mend]
        
        MACROSTATEMENT = %w[%abort %display %do %else %end %for %global %global %if %include %macro %mend
                            %nrstr %put %quote %str %sysfunc %then %to %unquote %until %while %window]
                 
        # for a complete list, see http://support.sas.com/documentation/cdl/en/allprodsproc/63875/HTML/default/viewer.htm#a003135046.htm
        # for extras included here (run, quit, etc.), see https://gist.github.com/cjdinger/7cf251399ef29b9b90b324a6fc442fca      
        PROCNAME = %w[access aceclus allele anom anova append appsrv arima autoreg bmdp bom 
                      boxplot btl build calendar calis callrfc cancorr candisc capability 
                      casecontrol catalog catmod cdisc chart cimport clp cluster compare 
                      compile computab contents convert copula copy corr corresp countreg cpm 
                      cport cusum cv2view data datasets datasource db2ext db2util dbcstab dbf 
                      dbload define_event define_tagset dif discrim display distance docparse 
                      document download dqmatch dqscheme dqsrvadm dqsrvsvc dtree entropy esm 
                      expand explode export factex factor family fastclus fcmp fmm fontreg 
                      forecast format forms freq fsbrowse fsedit fsletter fslist fsview g3d 
                      g3grid ga gam ganno gantt gareabar gbarline gchart gcontour gdevice 
                      geneselect genmod geocode gfont gimport ginside gis gkeymap gkpi 
                      glimmix glm glmmod glmpower glmselect gmap goptions gplot gproject gradar 
                      greduce gremove greplay groovy gslide gtestit gtile haplotype hpcountreg 
                      hpdmdb hpds2 hpf hpfarimaspec hpfdiagnose hpfengine hpfesmspec hpfevents 
                      hpfexmspec hpfidmspec hpforest hpfreconcile hpfselect hpfucmspec hplmixed 
                      hplogistic hpmixed hpneural hpnlin hpreduce hpreg hpsample hpseverity 
                      hpsummary htsnp http iml import inbreed infomaps intpoint ishikawa items 
                      javainfo kde krige2d lattice lifereg lifetest loan loess logistic lp 
                      macontrol macro mapimport mcmc mdc mddb mds means mend metadata metalib 
                      metaoperate mi mianalyze migrate mixed modeclus model multtest nested 
                      netdraw netflow nlin nlmixed nlp npar1way olap olapoperate operate 
                      optex options optload optlp optmilp optmodel optqp optsave orthoreg 
                      panel pareto pdlreg pds pdscopy phreg plan plm plot pls pm pmenu power 
                      princomp prinqual print printto probit proc proto prtdef prtexp psmooth 
                      pwencode qdevice qlim quantreg quest quit rank rdc rdpool rdsec reg 
                      registry release reliability report risk robustreg rsreg run scaproc 
                      score seqdesign seqtest server severity sgdesign sgpanel sgplot sgrender 
                      sgscatter shewhart sim2d similarity simlin simnormal soap sort source 
                      spectra sql standard statespace statgraph stdize stepdisc stp summary 
                      surveyfreq surveylogistic surveymeans surveyphreg surveyreg surveyselect 
                      syslin tabulate tapecopy tapelabel tcalis template timeid timeplot 
                      timeseries tpspline trans transpose transreg trantab tree tscsreg 
                      tspl ttest ucm univariate upload userproc varclus varcomp variogram 
                      varmax vaxtointeg webmddb x11 x12 xsl]
                       
        STATEMENT = %w[abort array attrib axis by class endrsubmit file filename footnote format freq goptions 
                       infile informat killtask legend libname listtask model note ods options pattern rdisplay 
                       rget rsubmit select signoff signon symbol sysecho systask table title waitfor where 
                       weight  xaxis yaxis xaxis2 yaxis2]
                       
        KEYWORD = %w[_all_ add alter array attrib axis bandplot barchart barchartparm 
                     bihistogram3dparm blockplot boxplot boxplotparm break by cards cards4 
                     class clear close column columns compute continuouslegend contourplotparm 
                     data data declare define densityplot describe disconnect discretelegend 
                     distinct dm drop drop dropline dynamic edit edit ellipse ellipseparm 
                     else endcomp entry entryfootnote entrytitle excel execute filename 
                     footnote format format freq fringeplot from goptions graphics group 
                     histogram histogramparm html html html5 id if informat input insert 
                     into keep killtask lineparm listing listing listtask loessplot merge 
                     model modelband needleplot nloptions ods options options parmcards 
                     parmcards4 pbsplineplot powerpoint proc proc put rand ranks rbreak 
                     rdisplay referenceline regressionplot replace reset retain rget rtf run 
                     scatterplot scatterplotmatrix seriesplot set signoff signon stepplot 
                     style surfaceplotparm symbol sysecho table tables tagsets then title 
                     title unique update validate value var var vectorplot waitfor weight where]
                     
        FUNCTION = %w[abs addr addrlong airy allcomb allcombi allperm anyalnum anyalpha 
                      anycntrl anydigit anyfirst anygraph anylower anyname anyprint anypunct 
                      anyspace anyupper anyxdigit arcos arcosh armend armgtid arminit armjoin 
                      armproc armstop armstrt armupdt arsin arsinh artanh ascebc atan atan2 
                      attrc attrn band beta betainv blackclprc blackptprc blkshclprc blshift 
                      bnot bor bquote brshift bxor byte cat catq cats catt catx cdf ceil 
                      ceilz cexist char choosec choosen cinv close cmiss cmpres cnonct 
                      coalesce coalescec collate comb compare compbl compcost compged complev 
                      compound compress compstor constant convx convxp cos cosh count countc 
                      countw css curobs cv daccdb daccdbsl daccsl daccsyd dacctab dairy datatyp 
                      datdif date datejul datepart datetime day dclose dcreate delete depdb 
                      depdbsl depsl depsyd deptab dequote deviance dhms dif digamma dim dinfo 
                      divide dnum dopen doptname doptnum dqcase dqgender dqgenderinfoget 
                      dqgenderparsed dqidentify dqlocaleguess dqlocaleinfoget dqlocaleinfolist 
                      dqmatch dqmatchinfoget dqmatchparsed dqparse dqparseinfoget dqparsetokenget 
                      dqparsetokenput dqpattern dqschemeapply dqsrvarchjob dqsrvcopylog 
                      dqsrvdeletelog dqsrvjobstatus dqsrvkilljob dqsrvprofjobfile dqsrvprofjobrep 
                      dqsrvuser dqstandardize dqtoken dread dropnote dsname dur durp ebcasc 
                      effrate envlen erf erfc euclid eval exist exp fact fappend fclose fcol 
                      fdelete fetch fetchobs fexist fget fileattr fileexist filename fileref 
                      finance find findc findfile findw finfo finv fipname fipnamel fipstate 
                      first floor floorz fnonct fnote fopen foptname foptnum fpoint fpos fput 
                      fread frewind frlen fsep fuzz fwrite gaminv gamma garkhclprc garkhptprc 
                      gcd geodist geomean geomeanz getdvi getjpi getlog getmsg getoption getquota 
                      getsym getterm getvarc getvarn graycode grdsvc_enable grdsvc_getaddr 
                      grdsvc_getinfo grdsvc_getname grdsvc_nnodes harmean harmeanz hbound hms 
                      holiday hour htmldecode htmlencode ibessel ifc ifn index indexc indexw 
                      input inputc inputn int intcindex intck intcycle intfit intfmt intget 
                      intindex intnx intrr intseas intshift inttest intz invcdf iorcmsg iqr irr 
                      isnull jbessel juldate juldate7 kurtosis label lag largest lbound lcm lcomb 
                      left length lengthc lengthm lengthn lexcomb lexcombi lexperk lexperm lfact 
                      lgamma libname libref limmoment log log10 log1px log2 logbeta logcdf logistic 
                      logpdf logsdf lowcase lperm lpnorm mad margrclprc margrptprc max md5 mdy mean 
                      median min minute missing missing mod module modulec modulen modz month mopen 
                      mort msplint mvalid n netpv nliteral nmiss nodename nomrate notalnum notalpha 
                      notcntrl notdigit note notfirst notgraph notlower notname notprint notpunct 
                      notspace notupper notxdigit npv nrbquote nrquote nrstr nvalid nwkdom open 
                      ordinal pathname pctl pdf peek peekc peekclong peeklong perm point poisson 
                      poke pokelong probbeta probbnml probbnrm probchi probf probgam probhypr 
                      probit probmc probnegb probnorm probt propcase prxchange prxdebug prxfree 
                      prxmatch prxnext prxparen prxparse prxposn prxsubstr ptrlongadd put putc 
                      putlog putn putsym pvp qcmpres qleft qlowcase qscan qsubstr qsysfunc qtr 
                      qtrim quantile quote qupcase ranbin rancau rand ranexp rangam range rank 
                      rannor ranperk ranperm ranpoi rantbl rantri ranuni read_array rename repeat 
                      resolve reverse rewind right rms round rounde roundz run_macro run_sasfile 
                      saving savings scan scanq sdf second set setterm sign sin sinh skewness sleep 
                      smallest soapweb soapwebmeta soapwipservice soapwipsrs soapws soapwsmeta 
                      softmax solve sortc sortn soundex spedis sqrt squantile std stderr stdize 
                      stfips stname stnamel str streaminit strip subpad substr substrn sum sumabs 
                      superq symexist symget symglobl symlocal symput symputx sysevalf sysexist 
                      sysfunc sysget sysmsg sysparm sysprocessid sysprocessname sysprod sysrc 
                      system tan tanh termin termout time timepart timevalue tinv tnonct today 
                      translate transtrn tranwrd trigamma trim trimn trunc ttclose ttcontrl 
                      ttopen ttread ttwrite uniform unquote upcase urldecode urlencode uss 
                      uuidgen var varfmt varinfmt varlabel varlen varname varnum varray varrayx 
                      vartype verify vformat vformatd vformatdx vformatn vformatnx vformatw 
                      vformatwx vformatx vinarray vinarrayx vinformat vinformatd vinformatdx 
                      vinformatn vinformatnx vinformatw vinformatwx vinformatx vlabel vlabelx 
                      vlength vlengthx vms vname vnamex vnext vtype vtypex vvalue vvaluex week 
                      weekday whichc whichn write_array year yieldp yrdif yyq zipcity zipcitydistance 
                      zipfips zipname zipnamel zipstate]
                      
    end
    
    def scan_tokens encoder, options

      # The scanner is always in a certain state, which is :initial by default.
      # We use local variables and symbols to maximize speed.
      state = :initial

      # stack, as a Ruby array
      stack = []

      # Define more flags and variables as you need them.
      key_expected = false

      # The main loop; eos? is true when the end of the code is reached.
      until eos?

        # Depending on the state, we want to do different things.
        case state

        # Normally, we use this case.
        when :initial
        
          # match white space
          if match = scan(/ \s+ /x)
            encoder.text_token match, :space
            
          # DATALINES/CARDS/LINES: http://sascommunity.org/wiki/DATALINES_statement
          if match = scan(/ (^|[\r\n])\s*(?:(?:data)?lines|cards|datalines4);[\s\S]+?[\r\n](\s)*; /i)
            encoder.text_token match, :dataline
            
          # comments: see http://sascommunity.org/wiki/Comment_statement
          elseif match = scan(/ (^\s*|;\s*)\*.*; /m) or match = scan(/\/\*[\s\S]+?\*\//)
              or match = scan(/ %(^\s*|;\s*)\*.*; /m)
            encoder.text_token match, :comment
          
          # dates/datetimes are LIKE strings, but not quite
          # "1Jan2016"d, '3:14:15pm't, '31jul2001:9:27:05am'dt
          elseif match = scan(/ ['"][^'"]+['"](?:d|d?t)\b /i)
            encoder.text_token match, :datetime
            
          # operators:
          elsif match = scan(/ \*\*|\|\||!!|¦¦|<>|><|[~¬^<>]?=|[*\/+\-<>&\|!¦~¬^]|\b(?:eq|ne|gt|lt|ge|le|in|not)\b /ix)
            encoder.text_token match, :operator
            
          # match number; SAS isn't picky about types,
          # but we need to care about that in format/informat specifications
          # can look like Decimal (1.2e23) or hexadecimal (0c1x)
          elsif match = scan(/ (?:\B-|\b)(?:[\da-f]+x|\d+(?:\.\d+)?(?:e[+-]?\d+)?) /ix)
            encoder.text_token match, :number
            
          elsif match = scan(/["']/)
            # A "quoted" token was found, and we know whether it is a key or a string.
            state = :string
            # This opens a token group and encodes the delimiter token.
            encoder.begin_group state
            encoder.text_token match, :delimiter
            
          elseif match = scan( /[$%@.(){}\[\];,\\]/ )
            encoder.text_token match, :punctuation
            
          else
            # If we reach invalid code, we try to discard
            # chars one by one and mark them as :error.
            encoder.text_token getch, :error
          end

        # String scanning is a bit more complicated, so we use another state for it.
        # The scanner stays in :string state until the string ends or an error occurs.
        when :string
        
          # Another if-elsif-else-switch, for strings this time.
          if match = scan(/[^\\"']+/)
            # Everything that is not \ or " is just string content.
            # includes new lines in SAS
            encoder.text_token match, :content
            
          elsif match = scan(/["']/)
            # A " or ' is found, which means this string is ending here.
            # A special token class, :delimiter, is used for tokens like this one.
            encoder.text_token match, :delimiter
            # Always close your token groups using the right token kind!
            encoder.end_group state
            # We're going back to normal scanning here.
            state = :initial
            
          elsif match = scan(/ \\ (?: #{ESCAPE} | #{UNICODE_ESCAPE} ) /mox)
            # A valid special character should be classified as :char.
            
            encoder.text_token match, :char
          elsif match = scan(/\\./m)
            # Anything else that is escaped (including \n, we use the m modifier) is
            # just content.
            encoder.text_token match, :content
            
          else
            # Nice for debugging. Should never happen.
            raise_inspect "else case \" reached; %p not handled." % [peek(1)], encoder
          end

        else
          # Nice for debugging. Should never happen.
          raise_inspect 'Unknown state: %p' % [state], encoder

        end
      end

      # If we still have a string or key token group open, close it.
      if [:string, :key].include? state
        encoder.end_group state
      end

      # Return the encoder.
      encoder
    end

  end

end
end
