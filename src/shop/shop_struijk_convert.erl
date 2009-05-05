%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-03
%%
%% @doc Convert the data from the Struijk dump to Zophrenic resources and shop_sku's

-module(shop_struijk_convert).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    convert/0
]).

-include_lib("zophrenic.hrl").

convert() ->
    Context = zp_context:new(),
    convert_trans(Context),
    ok = zp_db:transaction(fun convert_trans/1, Context),
    m_category:renumber(Context).
 
  
convert_trans(Context) ->
    ConvertF = fun(Ctx) ->
        % Read all data
        Rs = zp_db:q("select * from z_export order by id", Ctx),
        [ import(R, Ctx) || R <- Rs ],
        % throw({rollback, rollback_at_end_of_conversion}),
        ok
    end,
    zp_acl:sudo(ConvertF, Context).


import({Id, RootName, Name, Variant, Brand, Descr, NormalPrice, Price, _ArticleNrRaw, ArticleNr, Stock, Group}, Context) ->
    % Figure out what the normal and what the sale price is
    NormalPrice1 = to_price(NormalPrice),
    Price1 = to_price(Price),
    {SpecialPriceIncl, PriceIncl} = case NormalPrice1 of
        0 -> {0, Price1};
        _ -> {Price1, NormalPrice1}
    end,

    Stock1 = case Stock of
        undefined -> 0;
        N when N < 0 -> 0;
        N -> N
    end,
    Group1 = case Group of undefined -> <<>>; _ -> Group end,
    Brand1 = case Brand of undefined -> <<>>; _ -> Brand end,

    Variant1 = string:strip(zp_convert:to_list(Variant)),

    % Add the sku
    case zp_db:q1("select id from shop_sku where article_nr = $1", [ArticleNr], Context) of
        undefined ->
            RscId = import_rsc(Id, Name, Variant, Descr, Brand, Group, Context),
            MediaId = import_media(RscId, Id, Name, Variant, RootName, Context),
            
            % Create the Sku, referring to the resource
            Props = [
                {rsc_id, RscId},
                {media_id, MediaId},
                {available, true},
                {description1, Name},
                {variant, Variant1},
                {stock_avail, Stock1},
                {article_nr, ArticleNr},
                {stock, Stock1},
                {price_incl, PriceIncl},
                {price_excl, excl(PriceIncl)},
                {special_price_incl, SpecialPriceIncl},
                {special_price_excl, excl(SpecialPriceIncl)},
                {brand, Brand1},
                {image_file, RootName},
                {product_group, Group1}
            ],
            Props1 = case SpecialPriceIncl of
                0 -> 
                    [ {price_actual, PriceIncl} | Props];
                _ ->
                    [
                    {is_special, true},
                    {price_actual, SpecialPriceIncl},
                    {special_start, date()},
                    {special_end, {2009,12,31}}
                    | Props]
            end,

            {ok, _SkuId} = zp_db:insert(shop_sku, Props1, Context),
            ok;
        _ ->
            ok
    end.


import_rsc(Id, Name, _Variant, Descr, Brand, Group, Context) ->
    IdName = "prod_"++integer_to_list(Id),
    case zp_db:q1("select id from rsc where name = $1", [IdName], Context) of
        undefined ->
            % Make sure category exists
            CatId = case Group of
                undefined ->
                    m_category:name_to_id_check(parts, Context);
                _ ->
                    CatName = zp_string:to_name(Group),
                    case m_category:name_to_id(CatName, Context) of
                        {ok, N} ->
                            N;
                        {error, _} ->
                            % Make a new category
                            CP = [
                                {name, CatName},
                                {parent_id, m_category:name_to_id_check(accessories, Context)},
                                {title, Group}
                            ],
                            {ok, N} = m_category:insert(CP, Context),
                            N
                    end
            end,
            
            % Make a new resource, linking the filename and brand
            Props = [
                {name, IdName},
                {visible_for, 0},
                {is_published, true},
                {is_authoritative, true},
                {title, {trans, [{nl, Name}]}},
                {body, {trans, [{nl, Descr}]}},
                {category_id, CatId},
                {group_id, 1}
            ],
            {ok, RscId} = m_rsc:insert(Props, Context),
            
            % add brand (Brand)
            case Brand of
                undefined ->
                    ok;
                _ ->
                    BrandName = zp_string:to_name(Brand),
                    BrandId = case m_rsc:name_to_id(BrandName, Context) of
                        {ok, BId} -> 
                            BId;
                        _ ->
                            BrandProps = [
                                {name, BrandName},
                                {visible_for, 0},
                                {is_published, true},
                                {is_authoritative, true},
                                {title, {trans, [{nl, Brand}]}},
                                {category_id, m_category:name_to_id_check(brand, Context)},
                                {group_id, 1}
                            ],
                            {ok, BId} = m_rsc:insert(BrandProps, Context),
                            BId
                    end,
                    m_edge:insert(RscId, brand, BrandId, Context)
            end,
            RscId;

        RscId ->
            RscId
    end.


import_media(RscId, Id, Name, Variant, Filename, Context) -> 
    % add media (Filename)
    Fs = [
            {filename:join([code:lib_dir(zophrenic), "..", "struijk_import", "sproduct_orrigineel", [binary_to_list(Filename), ".jpg"]]), [$s|binary_to_list(Filename)]},
            {filename:join([code:lib_dir(zophrenic), "..", "struijk_import", [integer_to_list(Id), ".jpg"]]), [$p|integer_to_list(Id)]}
    ],
    Fs1 = lists:filter(fun({Fn, _Nm}) -> filelib:is_regular(Fn) end, Fs),
    case Fs1 of
        [{ImageFile, MediaName}|_] ->
            %% Check if the filename has been imported already
            MediaId = zp_db:q1("select id from media where name = $1", [MediaName], Context),
            case MediaId of
                undefined ->
                    % Upload the file, connect to the resource
                    % @todo, only connect the first found file, others should come indirectly via the Sku
                    {ok, NewMediaId} = m_media:insert_file_rsc(ImageFile, RscId, [{name, MediaName}, {title, [Name, " — ", Variant]}], Context),
                    NewMediaId;
                _ ->
                    % Already uploaded, connect to the resource (when not already connected)
                    case m_media:get_rsc_media(RscId, MediaId, Context) of
                        [] -> {ok, _} = m_media:insert_rsc_media(RscId, MediaId, Context);
                        _ -> nop
                    end,
                    MediaId
            end;
        [] ->
            undefined
    end.



to_price(<<>>) ->
    0;
to_price(T) ->
    S = binary_to_list(T),
    case string:tokens(S, ",.") of
        [A,B] -> list_to_integer(A) * 100 + list_to_integer(B);
        [A] -> list_to_integer(A) * 100
    end.

excl(Price) ->
    round(Price / 1.19).

%    create table z_export as
%    select p.id, 
%    	s.id::character varying as rootname, 
%    	p.naam, 
%    	s.naam as variant, 
%    	m.naam as merk, 
%    	p.omschrijving, 
%    	s.nprijs, 
%    	s.prijs, 
%    	s.jnr,
%    	rtrim(ltrim(replace(upper(s.jnr), ' ', ''), '-'), '-') as jnr2,
%    	v.aantal, 
%    	g.naam as groep
