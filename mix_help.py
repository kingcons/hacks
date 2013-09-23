#!/usr/bin/env python

import itertools
import os
import pickle
import time
from pyechonest import config
from pyechonest import song
from pyechonest import track
from pyechonest import util
from subprocess import check_output

# NOTE: We treat echonest's c-sharp as d-flat for the camelot mixing system.
camelot_scale = {'Ab minor': '1A',  'B major': '1B',
                 'Eb minor': '2A',  'F# major': '2B',
                 'Bb minor': '3A',  'C# major': '3B',
                 'F minor': '4A',   'Ab major': '4B',
                 'C minor': '5A',   'Eb major': '5B',
                 'G minor': '6A',   'Bb major': '6B',
                 'D minor': '7A',   'F major': '7B',
                 'A minor': '8A',   'C major': '8B',
                 'E minor': '9A',   'G major': '9B',
                 'B minor': '10A',  'D major': '10B',
                 'F# minor': '11A', 'A major': '11B',
                 'C# minor': '12A', 'E major': '12B'}
chromatic_scale = ['C', 'C#', 'D', 'Eb', 'E', 'F',
                   'F#', 'G', 'Ab', 'A', 'Bb', 'B']
mode_enum = ['minor', 'major']
track_metadata = {}
track_missing = []
mixtape_files = []
PICKLE_BACKUP = os.path.expanduser('~/.mixtape_data')

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
        track_missing.append(path)
        print "Couldn't find track '%s'. Try song.search?" % shortname

def group_by(n, iterable):
    args = [iter(iterable)] * n
    return itertools.izip_longest(*args)

def get_echonest_data():
    # It would be better to rate limit with a context manager here.
    for group in group_by(10, mixtape_files):
        for path in filter(None, group):
            get_track_data(path)
        time.sleep(60)

def display_metadata(path, tempo, key, mode):
    formatter = "{0} is {1} bpm, Camelot block {2}; ({3})."
    key_mode = "{0} {1}".format(chromatic_scale[key], mode_enum[mode])
    print formatter.format(path, tempo, camelot_scale[key_mode], key_mode)

def show_results():
    print "\n---- Results ----\n"
    for path, metadata in track_metadata.items():
        if isinstance(metadata, track.Track):
            display_metadata(path, metadata.tempo, metadata.key, metadata.mode)
        else:
            metahash = metadata.audio_summary
            display_metadata(path, metahash['tempo'], metahash['key'], metahash['mode'])

## Helpers

def save_metadata():
    with open(PICKLE_BACKUP, 'w') as f:
        pickle.dump(track_metadata, f)

def load_metadata():
    with open(PICKLE_BACKUP, 'r') as f:
        return pickle.load(f)

def create_track_from_path(path):
    try:
        return track.track_from_filename(path)
    except util.EchoNestAPIError:
        print "Could not create track from file"

def main():
    add_mixtape_songs()
    get_echonest_data()
    show_results()

initialize()
from IPython import embed; embed()
