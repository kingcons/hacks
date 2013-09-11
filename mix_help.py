#!/usr/bin/env python
import time
from pyechonest import config
from pyechonest import song
from pyechonest import track
from pyechonest import util

track_metadata = []
track_uploads = []

with open('/home/redline/.echonest', 'r') as f:
    config.ECHO_NEST_API_KEY = f.readline()

with open('/home/redline/.mixtape', 'r') as f:
    text = f.read()
    lines = text.split('\n')
    tracks = [s.split(' - ') for s in lines if s != '']

def song_lookup(artist, title):
    try:
        search = song.search(artist=artist, title=title)[0]
        track_metadata.append(search)
    except IndexError:
        print "Could not find match for %s, %s." % (artist, title)
        track_uploads.append("{0} - {1}".format(artist, title))
#        print "Please enter the path of the file to analyze: "
#        path = raw_input()

def create_from_path(path):
    try:
        result = track.track_from_filename(path)
        return result
    except util.EchoNestAPIError, e:
        print "Couldn't create track from file."
        return e

def fetch_track_data():
    group_by_5 = zip(*[iter(tracks)]*5)
    for group in group_by_5:
        for artist, title in group:
            song_lookup(artist, title)
        time.sleep(60)

def track_tempo(obj):
    return obj.audio_summary['tempo']

def show_sorted_tracks(tracks):
    for t in sorted(tracks, key=track_tempo):
        print "{0} - {1} has tempo {2}".format(t.artist_name,
                                               t.title,
                                               t.audio_summary['tempo'])

fetch_track_data()
print
show_sorted_tracks(track_metadata)
from IPython import embed; embed()
