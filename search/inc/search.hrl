%% define record
-record(app,{name,
              version,
              des
             }).
-record(mod,{name,
             app,             
             des
            }).
-record(func,{name,
              mod,
              arg,
              des
             }).
