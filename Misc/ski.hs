#!/usr/bin/env stack
-- stack script --resolver=lts-20.13 --package random
import Import;
s=78;
ll=6;
rl=s-6;
c=s`div`2;
w=24;
m=2;
t='🌲';
h=(`div`2);
q=p.show
ra=randomRIO;
rp=replicate;
rm=replicateM_;
d=threadDelay 50000;
p s=putStrLn s>>d
sc=rm 10 $ p"";
main=hSetEcho stdin False>>hSetBuffering stdin NoBuffering>>p"Ski!"
 >>lp 0 c c;lp o x c=do{let{l0=c-h w;(l,ml)=if l0<ll then(ll,0)else(l0,m);r0=c+h
w+1;(r,mr)=if r0>rl then(rl,0)else(r0,m);};j<-hReady stdin;i<-(if j then getChar
else return '-');let{lt=h$l-1;rt=h$s-r-1;ls=x-lt*2;rs=s-rt*2-x-1;k=if min ls rs
<1 then"*"else"V"} in do {p(take s$rp lt t++rp ls ' '++k++rp rs ' '++rp rt t);if
k/="*"then lp(o+1)
    (case i of 'a'->x-1;'d'->x+1;_->x).(c+)=<<ra(-ml,mr)else q o}}
