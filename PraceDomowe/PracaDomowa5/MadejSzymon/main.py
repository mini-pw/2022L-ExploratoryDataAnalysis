from flask import Flask, request, session, g, redirect, \
    url_for, abort, render_template, flash
import pandas as pd
import json
import plotly

app = Flask(__name__)


@app.route("/")
def index():
    mapbox_access_token = 'pk.eyJ1Ijoic3ptbm1kaiIsImEiOiJjbDMza2MyY2YwOGI1M29uMTA4cDRtdTlpIn0.00j47lPGa_op79WqgFTfSw'
    cond = ['price', 'bedrooms', 'bathrooms', 'sqft_living', 'sqft_lot', 'floors', 'waterfront', 'view', 'condition']
    df = pd.read_csv('static/seattle.csv')
    return render_template('index.html',
                           data =df_to_geojson(df, cond),
                           ACCESS_KEY = mapbox_access_token)


def data():
    df = pd.read_csv('static/seattle.csv')
    to_json_object = df[['price', 'lat', 'long']]
    return to_json_object.to_json(orient= 'records')

def df_to_geojson(df, properties):
    geojson = {'type':'FeatureCollection', 'features':[]}
    for _, row in df.iterrows():
        feature = {'type':'Feature',
                   'properties':{},
                   'geometry':{'type':'Point',
                               'coordinates':[]}}
        feature['geometry']['coordinates'] = [row['long'],row['lat']]
        for prop in properties:
            feature['properties'][prop] = row[prop]
        geojson['features'].append(feature)
    return geojson



if __name__ == '__main__':
    app.run(debug=True)