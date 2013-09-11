#!/usr/bin/env python
import time
from pyechonest import config
from pyechonest import song
from pyechonest import track
from pyechonest import util

chromatic_scale = ['C', 'C#', 'D', 'Eb', 'E', 'F',
                   'F#', 'G', 'Ab', 'A', 'Bb', 'B']
track_metadata = []
track_pending = []

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
        path = raw_input('Please enter the mp3 path: ')
        track_pending.append(create_from_path(path))

def fetch_track_data():
    group_by_5 = zip(*[iter(tracks)]*5)
    for group in group_by_5:
        for artist, title in group:
            song_lookup(artist, title)
        time.sleep(60)

def create_from_path(path):
    try:
        result = track.track_from_filename(path)
        return result
    except util.EchoNestAPIError, e:
        print "Couldn't create track from file."
        return e

def show_sorted_tracks(tracks):
    for t in sorted(tracks, key=lambda x: x.audio_summary['tempo']):
        meta = t.audio_summary
        formatter = "{0} - {1} has tempo {2}, key {3}"
        print formatter.format(t.artist_name,
                               t.title,
                               meta['tempo'],
                               chromatic_scale[meta['key']])

def main():
    fetch_track_data()
    print
    show_sorted_tracks(track_metadata)

main()
from IPython import embed; embed()
