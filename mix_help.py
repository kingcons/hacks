#!/usr/bin/env python

import os
import time
from pyechonest import config
from pyechonest import song
from pyechonest import track
from pyechonest import util
from subprocess import check_output

chromatic_scale = ['C', 'C#', 'D', 'Eb', 'E', 'F',
                   'F#', 'G', 'Ab', 'A', 'Bb', 'B']
mode_enum = ['minor', 'major']
track_metadata = []
track_pending = []

def initialize():
    with open(os.path.expanduser('~/.echonest'), 'r') as f:
        config.ECHO_NEST_API_KEY = f.readline()

def get_mix_md5s():
    with open(os.path.expanduser('~/.mixtape_files'), 'r') as f:
        files = f.readlines()
        md5sums = [check_output(['md5sum', f]) for f in files]
        return [md5.split()[0] for md5 in md5sums]

def get_mix_songs():
    with open(os.path.expanduser('~/.mixtape'), 'r') as f:
        songs = f.readlines()
        return [s.split(' - ') for s in songs if s != '']

def song_lookup(artist, title):
    try:
        search = song.search(artist=artist, title=title)[0]
        track_metadata.append(search)
    except IndexError:
        print "Could not find match for %s, %s." % (artist, title)
        path = raw_input('Please enter the mp3 path: ')
        track_pending.append(create_from_path(path))

def fetch_track_data(md5s):
    group_by_10 = zip(*[iter(md5s)]*10)
    for md5 in group_by_10:
        track_metadata.append(track.track_from_md5(md5))

def fetch_song_data(tracks):
    group_by_10 = zip(*[iter(tracks)]*10)
    for group in group_by_10:
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
        formatter = "{0} - {1} has tempo {2}, mode {3}, key {4}"
        print formatter.format(t.artist_name,
                               t.title,
                               meta['tempo'],
                               mode_enum[meta['mode']],
                               chromatic_scale[meta['key']])

def main():
    initialize()
    if os.path.isfile(os.path.expanduser('~/.mixtape_files')):
        md5s = get_mix_md5s()
        fetch_track_data(md5s)
    else:
        songs = get_mix_songs()
        fetch_song_data(songs)
    print
    show_sorted_tracks(track_metadata)
    print

main()
from IPython import embed; embed()
