ERLC=/usr/local/bin/erlc
ERLCFLAGS=-o
SRCDIR=./search/src
LOGDIR=/var/log/mysoftware
CONFDIR=/etc/mysoftware
BEAMDIR=./ebin

all: 
 @ mkdir -p $(BEAMDIR) ;
 @ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
 @ mkdir -p $(CONFDIR) ;
 @ mkdir -p $(LOGDIR) ;
 @ cp conf/mysoftware.conf $(CONFDIR)/mysoftware.conf-example
clean: 
 @ rm -rf $(BEAMDIR) ;
 @ rm -rf erl_crush.dump
