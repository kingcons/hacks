#!/usr/bin/env python

## TODO:
# In case of missing track, prompt for artist and title,
# then song.search and add result?

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
track_metadata = {}
mixtape_files = []

def initialize():
    with open(os.path.expanduser('~/.echonest'), 'r') as f:
        config.ECHO_NEST_API_KEY = f.readline()

def get_md5sum(path):
    return check_output(['md5sum', path]).split()[0]

def short_path(path):
    return os.path.basename(path)

def add_mixtape_song(path):
    if os.path.exists(path):
        mixtape_files.append(path)
    else:
        print "The file '%s' was not found." % path

def add_mixtape_songs():
    with open(os.path.expanduser('~/.mixtape_files'), 'r') as f:
        for path in f.read().splitlines():
            add_mixtape_song(path)

def get_track_data(path):
    try:
        md5 = get_md5sum(path)
        shortname = short_path(path)
        track_metadata[shortname] = track.track_from_md5(md5)
    except util.EchoNestAPIError:
        print "Couldn't find track '%s'. Try song.search?" % shortname

def get_echonest_data():
    # It would be better to rate limit with a context manager here.
    for group_by_10 in zip(*[iter(mixtape_files)]*10):
        for path in group_by_10:
            get_track_data(path)
        time.sleep(60)

def display_metadata(path, tempo, key, mode):
    formatter = "{0} is {1} bpm and is in {2} {3}."
    print formatter.format(path, tempo, chromatic_scale[key], mode_enum[mode])

def show_sorted_tracks():
    print "\n---- Results ----\n"
    for path, metadata in track_metadata.items():
        if isinstance(metadata, track.Track):
            display_metadata(path, metadata.tempo, metadata.key, metadata.mode)
        else:
            metahash = metadata.audio_summary
            display_metadata(path, metahash['tempo'], metahash['key'], metahash['mode'])

## Helpers

def create_track_from_path(path):
    try:
        return track.track_from_filename(path)
    except util.EchoNestAPIError:
        print "Could not create track from file"

def main():
    initialize()
    add_mixtape_songs()
    get_echonest_data()
    show_sorted_tracks()

main()
from IPython import embed; embed()
