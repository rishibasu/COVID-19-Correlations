MapLatLimit = [25 50];
MapLonLimit = [-145 -50];
disp("good 1")

UScounties = shaperead('tl_2017_us_county.shp', 'UseGeoCoords', true, ...
   'BoundingBox', [MapLonLimit' MapLatLimit']);
disp("good 2")

for k = 1:3093
    for j = 1:3093
        if UScounties(k).GEOID == pm(j,1)
            UScounties(k).pm = str2double(pm(j, 2));
        end
        pm(j, 2) = str2double(pm(j ,2);
    end
    
    UScounties(k).LabelLat = str2double(UScounties(k).INTPTLAT);
    UScounties(k).LabelLon = str2double(UScounties(k).INTPTLON);
    %counties(k).X = counties(k).INTPTLAT;
    %counties(k).Y = counties(k).INTPTLON;
end
%pm2 = str2double(pm(:,2));



disp("good 3")


axesm('MapProjection', 'eqaconic', 'MapParallels', [],...
  'MapLatLimit', MapLatLimit, 'MapLonLimit', MapLonLimit,...
  'GLineStyle', '-')
geoshow(UScounties, 'DisplayType', 'polygon', 'FaceColor','green')
disp("good 4")

maxpm = 40
disp("good 5")

fall = flipud(autumn(numel(UScounties)));
disp("good 6")

pmColors = makesymbolspec('Polygon', {'pm', ...
   [0 maxpm], 'FaceColor', fall});
disp("good 7")

% all good up to here
geoshow(UScounties, 'DisplayType', 'polygon', 'SymbolSpec', pmColors)
title ({'County Level 17-year Long=Term Average of PM2.5 Concentrations', ...
   'in the US in g/m^3'})
disp("good 8")

caxis([0 maxpm])
colormap(fall)
colorbar
disp("good 9")