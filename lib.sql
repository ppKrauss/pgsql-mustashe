-- 
-- Basic Mustache Template Engine to text templates and JSON or JSONb input data
--
-- see Mustache's specification v5 at https://mustache.github.io/mustache.5.html
--
-- Alternative: use a trusted language like Perl and its Mustache implementation.
--

-- -- -- -- -- -- -- -- -- -- -- -- -- --
-- -- Public library, general use:  -- --

CREATE or replace FUNCTION iIF(
    condition boolean,       -- IF condition
    true_result anyelement,  -- THEN
    false_result anyelement  -- ELSE
) RETURNS anyelement AS $f$
  SELECT CASE WHEN condition THEN true_result ELSE false_result END
$f$  LANGUAGE SQL IMMUTABLE;

CREATE or replace FUNCTION iIF_notnull(
    param1 anyelement,  onNull_result anyelement
) RETURNS anyelement AS $f$
  SELECT CASE WHEN param1 IS NOT NULL THEN param1 ELSE onNull_result END
$f$  LANGUAGE SQL IMMUTABLE;

CREATE or replace FUNCTION iIF_null(
    checknull anyelement,  onNull_result anyelement, otherwise anyelement
) RETURNS anyelement AS $f$
  SELECT CASE WHEN checknull IS NULL THEN onNull_result ELSE otherwise END
$f$  LANGUAGE SQL IMMUTABLE;


CREATE or replace FUNCTION agg_replace_fn(oldtxt text, txt text, fnd text, rpl text) RETURNS text AS $f$
  SELECT CASE
     WHEN oldtxt>'' AND txt>'' AND fnd>'' THEN replace( oldtxt, fnd, rpl )
     WHEN (oldtxt IS NULL OR oldtxt='') AND txt>'' AND fnd>'' THEN replace( txt, fnd, rpl )
     ELSE COALESCE(txt,'')
     END
$f$ LANGUAGE SQL IMMUTABLE;

CREATE or replace AGGREGATE agg_replace(text,text,text) (
  sfunc = agg_replace_fn,
  stype = text,
  initcond = '' -- NULL
);


-- -- -- -- -- -- -- --
-- -- Mustache:   -- --
CREATE or replace FUNCTION jsonb_text_mustache_placeholder(
  j JSONb, tpl text
  ,is_mustache boolean DEFAULT true -- false is URI-template
  ,prefix text DEFAULT ''  -- for javascript ${x} delimitor.
) RETURNS text AS $f$
      SELECT agg_replace(t2.r,placeholder,val)
      FROM (
       SELECT  tpl,
               iIF( is_mustache, '{{'||x[1]||'}}', prefix||'{'||x[1]||'}' ) as placeholder,
               COALESCE(j->>x[1],'') as val
       FROM regexp_matches(
               tpl,
               iIF(is_mustache,'\{\{([^#/\{\}]+)\}\}',IIF(prefix='','','\$')||'\{([^#/\{\}]+)\}'),  -- not blocks
               'g'
        ) taux1(x)
      ) t1 RIGHT JOIN (SELECT tpl AS r) t2 ON true
$f$ LANGUAGE SQL IMMUTABLE;

CREATE or replace FUNCTION jsonb_text_mustache(
  j JSONb,
  tpl text,
  r_sep text DEFAULT '', --  or E'\n'
  count_loops int DEFAULT 0
) RETURNS text AS $f$
       WITH blks AS (
        SELECT  x[1] vname, '{{#'||x[1]||'}}' op, '{{/'||x[1]||'}}' cl, x[2] as inner_blk
        FROM regexp_matches( tpl, '\{\{#([^\{\}]+)\}\}(.+?)\{\{/\1\}\}', 'g') t1(x)  -- only blocks
             RIGHT JOIN (SELECT true) nop1 ON true
       ), -- \blks
       blk_result AS (
        SELECT  replace(
                  tpl,
                  COALESCE( op||inner_blk||cl, '' ),  -- recursive:
                  string_agg( CASE
                    WHEN is_array OR is_true THEN r_sep||CASE
                       WHEN count_loops<10 THEN jsonb_text_mustache(jitem,inner_blk,r_sep,count_loops+1)
                       ELSE inner_blk
                       END
                    ELSE ''
                   END,  '')
                ) AS tpl_result
        FROM (
         SELECT *, jsonb_array_elements(
             CASE WHEN is_array THEN j->vname ELSE '[true]'::jsonb END
         ) AS jitem
         FROM blks, LATERAL (
           SELECT j?blks.vname AND jsonb_typeof(j->blks.vname)='array' AS is_array, 
                  j?blks.vname AND CASE
                     jsonb_typeof(j->blks.vname) WHEN 'boolean' THEN (j->vname)::boolean WHEN 'null' THEN false ELSE true
                  END AS is_true
         ) t3
        ) t4
        RIGHT JOIN (SELECT true) nop2 ON true
        GROUP BY tpl, op, inner_blk, cl
       ) -- \blk_result
       SELECT jsonb_text_mustache_placeholder(j,tpl_result)
       FROM blk_result
$f$ LANGUAGE SQL IMMUTABLE;
