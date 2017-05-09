import django, os, sys, time, resource, re, gc, shutil, string, nltk
from django.utils import timezone
from nltk import ngrams
import numpy as np
import subprocess

sys.path.append('/home/galm/software/tmv/BasicBrowser/')

os.environ.setdefault("DJANGO_SETTINGS_MODULE", "BasicBrowser.settings")
django.setup()

from scoping.models import *

def jaccard(s1,s2):
    try:
        return len(s1.intersection(s2)) / len(s1.union(s2))
    except:
        return 0


irs = IPCCRef.objects.filter(year__lt=2017,words__len__gt=0).order_by('-year')

for ir in irs.iterator():
    #print(ir.text)
    title = ir.text.split(".")[0].strip()
    l = len(title)

    timatches = Doc.objects.filter(
        title__icontains=title,tilength__lt=l+3,
        tilength__gt=l-3,PY=ir.year
    )
    if timatches.count() == 1:
        ir.doc=timatches.first()
        ir.save()
    else:
        posmatches = Doc.objects.filter(
            title__icontains=ir.words[0],
            tilength__gt=l-3,
            tilength__lt=l+3,
            PY = ir.year
        )
        for pm in posmatches:
            j = jaccard(ir.shingle(),pm.shingle())
            if j > 0.45:
                ir.doc=pm
                ir.save()
                break
