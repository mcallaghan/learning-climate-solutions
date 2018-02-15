import django, os, sys, time, resource, re, gc, shutil, string, nltk
from django.utils import timezone
from nltk import ngrams
import numpy as np
import subprocess
from multiprocess import Pool

sys.path.append('/home/galm/software/django/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

def jaccard(s1,s2):
    try:
        return len(s1.intersection(s2)) / len(s1.union(s2))
    except:
        return 0


irs = IPCCRef.objects.filter(
    year__lt=2017,
    words__len__gt=1,
    doc__isnull=True
).order_by('year')
    #print(ir.text)


def match_ir(ir):
    title = ir.text.split(".")[0].strip()
    l = len(title)

    if ir.year > 1900 and ir.year < 2017:
        timatches = Doc.objects.filter(
            title__icontains=title,tilength__lt=l+3,
            tilength__gt=l-3,PY=ir.year
        )
    else:
        timatches = Doc.objects.filter(
            title__icontains=title,tilength__lt=l+3,
            tilength__gt=l-3
        )
    if timatches.count() == 1:
        ir.doc=timatches.first()
        ir.save()
        return
    elif timatches.count() > 1:
        posmatches = timatches
    else:
        len_tolerance = 30
        if ir.year > 1900 and ir.year < 2017:
            posmatches = Doc.objects.filter(
                title__icontains=ir.words[0],
                tilength__gt=l-len_tolerance,
                tilength__lt=l+len_tolerance,
                PY = ir.year
            )
        else:
            posmatches = Doc.objects.filter(
                title__icontains=ir.words[0],
                tilength__gt=l-len_tolerance,
                tilength__lt=l+len_tolerance
            ).filter(
                title__icontains=ir.words[1]
            )
    for pm in posmatches:
        j = jaccard(ir.shingle(),pm.shingle())
        if j > 0.45:
            ir.doc=pm
            ir.save()
            break
    return

pool = Pool(processes=6)
pool.map(match_ir,irs)
pool.terminate()
