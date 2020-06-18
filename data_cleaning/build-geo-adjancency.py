
import datetime as dt
import pandas as pd
import os
import json
import sys
import logging
import numpy as np


class Stats:
    """
    Class that holds the stats for each county we are interested in. Use this
    rather than a dataframe b/c dataframe lookups were slow, dictionary was much faster.
    """
    def __init__(self):
        self.device_count = 0
        self.non_home_cbg_visits_within_county = 0
        self.cbg_visits_outside_county = 0
        self.home_cbg_visits = 0
        self.outside_device_cbg_visits = 0

    @staticmethod
    def export_column_names():
        return '\t'.join(['origin-county', 'date', 'device-count', 'non-home-cbg-visits-within-county',
                          'cbg-visits-outside-county', 'home-cbg-visits',
                          'outside-device-cbg-visits']) + '\n'

    def export_data(self, date, origin_county):
        return '\t'.join([str(origin_county), date, str(self.device_count), str(self.non_home_cbg_visits_within_county),
                          str(self.cbg_visits_outside_county), str(self.home_cbg_visits),
                          str(self.outside_device_cbg_visits)]) + '\n'


def get_logger(date, out_path):
    """
    Initialize a log for each day.
    :param date: Date to ingest
    :param out_path: directory where log is to be stored
    :return: logging.Logger object
    """
    logger = logging.getLogger('build-geo_{0}'.format(date.strftime('%Y%m%d')))
    logger.setLevel(logging.DEBUG)

    # create console handler and set level info
    handler = logging.StreamHandler()
    handler.setLevel(logging.INFO)
    logger.addHandler(handler)

    fn = os.path.join(out_path, '{0}_build_geo_log.txt'.format(date.strftime('%Y%m%d')))

    # create file handler
    handler = logging.FileHandler(fn, mode='w')
    handler.setLevel(logging.DEBUG)
    logger.addHandler(handler)
    return logger


def fips2county(fips):
    """
    Return first 5 digits of fips code to get county. Accepts 12 digit FIPS.
    :param fips: 12 digits fips code, must be string.
    :return: int - 5 digit fips code
    """
    return fips.zfill(12)[0:5]


def main(date):
    parent_dir = '/nfs/sloanlab004/projects/covid_mobility_proj/data/'
    raw_dir = os.path.join(parent_dir, 'safegraph_data', 'social-distancing_v2')
    out_path = os.path.join(parent_dir, 'PROCESSED_DATA', 'safe_graph_geos', str(date.year), str(date.month).zfill(2))
    if not os.path.exists(out_path):
        os.makedirs(out_path)
    log = get_logger(date, out_path=out_path)

    # read in data and add dates
    file = os.path.join(raw_dir, str(date.year), str(date.month).zfill(2), str(date.day).zfill(2),
                        '{0}-social-distancing.csv'.format(date.strftime('%Y-%m-%d')))
    df = pd.read_table(file, sep=',')
    df.date_range_start = pd.to_datetime(df.date_range_start)
    df.origin_census_block_group = df.origin_census_block_group.apply(lambda s: str(s).zfill(12))
    df['county'] = df.origin_census_block_group.apply(fips2county)

    # check some assumptions about the data
    assert len(df) == len(df.origin_census_block_group.unique())
    assert len(df.date_range_start.apply(lambda dd: dd.date()).unique()) == 1
    date = df.date_range_start.iloc[0].date()

    # Loop over all CBGs in the data to build our data set
    county_data = dict((el, Stats()) for el in df.county.unique())
    county_graphs = {}
    start_time = dt.datetime.now()
    for ix in df.index:
        if ix % 1000 == 0 or ix == len(df) - 1:
            print('Processing data for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
                date.strftime('%Y-%m-%d'), ix+1, len(df), (dt.datetime.now() - start_time).total_seconds() / 60))
        origin_cbg = df.loc[ix, 'origin_census_block_group']
        origin_county = fips2county(origin_cbg)
        assert origin_county == df.loc[ix, 'county']

        # This object contains Dict<dest_cbg, num-devices> representing the number of devices
        # from origin cbg to destination cbg. Field description pasted below.
        dic = json.loads(df.loc[ix, 'destination_cbgs'])

        # add number of devices in cbg to county dataset
        county_data[origin_county].device_count += df.loc[ix, 'device_count']

        if origin_county not in county_graphs:
            county_graphs[origin_county] = {}

        # loop over destination cbgs and build the objects
        for dest_cbg in dic:
            dest_county = fips2county(dest_cbg)
            if dest_cbg == origin_cbg:
                county_data[origin_county].home_cbg_visits += dic[dest_cbg]
                continue
            elif dest_county == origin_county:
                county_data[origin_county].non_home_cbg_visits_within_county += dic[dest_cbg]
            else:
                county_data[origin_county].cbg_visits_outside_county += dic[dest_cbg]

            # now add to outside visits to dest county
            if origin_county != dest_county:
                if dest_county not in county_data:
                    # This rarely, but occasionally, happens. Represents no home devices within a county but devices go to that county.
                    county_data[dest_county] = Stats()
                county_data[dest_county].outside_device_cbg_visits += dic[dest_cbg]

            # add to county graphs (this excludes devices staying within home cbg b/c of continue above)
            if dest_county not in county_graphs[origin_county]:
                county_graphs[origin_county][dest_county] = 0
            county_graphs[origin_county][dest_county] += dic[dest_cbg]
    log.info('Processing data for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
        date.strftime('%Y-%m-%d'), len(df), len(df), (dt.datetime.now() - start_time).total_seconds() / 60))

    # make data frame from graph dict
    start_time = dt.datetime.now()
    log.info("Building county graph now")
    county_graph_df = pd.DataFrame(
        index=range(np.sum([len(county_graphs[el]) for el in county_graphs])),
        columns=['origin-county', 'dest-county', 'num-devices']
    )
    ix = 0
    for origin_county in county_graphs:
        if ix % 1000 == 0 or ix == len(df) - 1:
            print('Building county graph for date {0}: {1} / {2} ({3:.2f} minutes)'.format(
                date.strftime('%Y-%m-%d'), ix+1, len(df), (dt.datetime.now() - start_time).total_seconds() / 60))
        for dest_county in county_graphs[origin_county]:
            county_graph_df.loc[ix, 'origin-county'] = origin_county
            county_graph_df.loc[ix, 'dest-county'] = dest_county
            county_graph_df.loc[ix, 'num-devices'] = county_graphs[origin_county][dest_county]
            ix += 1
    log.info('Done building graph. ({0:.2f} minutes)'.format((dt.datetime.now() - start_time).total_seconds() / 60))

    # write data
    start_time = dt.datetime.now()
    log.info("Writing data now")
    county_graph_fn = os.path.join(out_path, '{0}_county_graph.txt'.format(date.strftime('%Y%m%d')))
    county_graph_df.to_csv(county_graph_fn, sep='\t')

    # write the county level data set.
    county_fn = os.path.join(out_path, '{0}_county_geos.txt'.format(date.strftime('%Y%m%d')))
    date_str = date.strftime('%Y-%m-%d')
    with open(county_fn, 'w') as f:
        f.write(Stats.export_column_names())
        for el in county_data:
            f.write(county_data[el].export_data(date_str, origin_county=el))
    log.info('Done writing data. ({0:.2f} minutes)'.format((dt.datetime.now() - start_time).total_seconds() / 60))


if __name__ == '__main__':
    """
    Call this script w/ days from 2020-1-1 as command line arguments. I.e. python .../build-geo-adjacency.py "0" "31"
    parses the 31 days since 2020-1-1, including 2020-1-1 as 0.
    """
    job_start_num = int(sys.argv[1]) - 1
    if len(sys.argv) == 3:
        job_end_num = int(sys.argv[2]) - 1
    else:
        job_end_num = job_start_num
    d = dt.datetime(2020, 1, 1) + dt.timedelta(days=job_start_num)
    job_end_date = dt.datetime(2020, 1, 1) + dt.timedelta(days=job_end_num)
    while d <= job_end_date:
        main(d)
        d = d + dt.timedelta(days=1)


# ### DESCRIPTION OF destination_cbgs ###############
# Key is a destination census block group and value is the number
# of devices with a home in census_block_group that stopped in the
# given destination census block group for >1 minute during the time period.
# Destination census block group will also include the origin_census_block_group
# in order to see if there are any devices that originate from
# the origin_census_block group but are staying completely outside of it.])
