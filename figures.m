pm = aggregatepmcensuscdctest(:,(2:3));
pm = table2array(pm);

latlim = [25 50];
lonlim = [-145 -50];

%counties = shaperead('tl_2017_us_county.shp');
counties = shaperead('tl_2017_us_county.shp', 'UseGeoCoords', true, ...
   'BoundingBox', [lonlim' latlim']);





%getm(gca,'MapProjection');
%for k = 1:numel(counties)
   % counties(k).FIPS = strcat(counties(k).STATEFP, counties(k).COUNTYFP);
%end
     

%where 
for k = 1:3093
    for j = 1:3093
        if counties(k).GEOID == pm(j,1)
            counties(k).pm = pm(j, 2);
        end
    end
    counties(k).X = counties(k).INTPTLAT;
    counties(k).Y = counties(k).INTPTLON;
end
pm2 = pm(:,2);
%bruh
%surfaceColors = makesymbolspec('Polygon', {'pm2', [min([counties.pm(2)]) max([counties.pm(2)])], 'FaceColor', autumn(numel(counties)) });
disp("good start")
%geoshow(counties,'DisplayType', 'polygon', 'SymbolSpec', surfaceColors)


%-----
%load ToUpload
figure('Color','white')
usamap([25 50],[-145 -50])
colormap(parula)

surfaceColors = makesymbolspec('Polygon', {'pm2',[0 20],'FaceColor',colormap});
disp("good colors")
%fix the geoshow function
geoshow(counties,'DisplayType', 'polygon', 'SymbolSpec', surfaceColors)
disp("good geoshow")
%geoshow(counties,'SymbolSpec',surfaceColors)
colorbar
caxis([0 20])
set(get(colorbar,'XLabel'),'String','PM 2.5')
disp("done")