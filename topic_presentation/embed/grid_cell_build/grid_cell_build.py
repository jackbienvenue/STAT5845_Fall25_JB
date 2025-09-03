import geopandas as gpd
from shapely.geometry import Point, Polygon
import cfgrib
import folium
import warnings
warnings.filterwarnings("ignore")

# Load data from GRIB
file_path = "data/download_ERA5_LAND_package_1979_01.grib"
hourly_data = cfgrib.open_dataset(
    file_path,
    backend_kwargs={'filter_by_keys': {'typeOfLevel': 'surface', 'step': 1}}
)
df = hourly_data.to_dataframe().reset_index()
data = df[['latitude', 'longitude']].drop_duplicates()
data = data.rename(columns={'latitude': 'lat', 'longitude': 'lon'})

# Create points and GeoDataFrame
geometry = [Point(lon, lat) for lon, lat in zip(data['lon'], data['lat'])]
gdf = gpd.GeoDataFrame(data, geometry=geometry, crs="EPSG:4326")

# Create grid cells
grid_size = 0.1
def create_grid_cell(center_point, size):
    lat, lon = center_point.y, center_point.x
    return Polygon([
        (lon - size/2, lat - size/2), (lon + size/2, lat - size/2),
        (lon + size/2, lat + size/2), (lon - size/2, lat + size/2)
    ])

grid_cells = [create_grid_cell(pt, grid_size) for pt in gdf.geometry]
gdf_grid_cells = gpd.GeoDataFrame(geometry=grid_cells, crs="EPSG:4326")

# Create Folium map
m = folium.Map(location=[
    gdf_grid_cells.geometry.centroid.y.mean(),
    gdf_grid_cells.geometry.centroid.x.mean()
], zoom_start=12)

folium.GeoJson(gdf_grid_cells).add_to(m)

# Save the map to an HTML file
m.save("grid_map.html")