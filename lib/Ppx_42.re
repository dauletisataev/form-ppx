
open Migrate_parsetree;
open Ast_409;
open Ast_mapper;
open Ast_helper;
open Ast_convenience_409;
open Location;
open Parsetree;
open Longident;

let nllid = name => mknoloc(Lident(name));
let mkref = name => Exp.mk(Pexp_ident(mknoloc(Lident(name))));
let mklet = (name, expr) => Vb.mk(pvar(name), expr);

let updatedRecord = (~singleField=false, record, field, value) =>
  Exp.mk(
    Pexp_record(
      [(nllid(field), mkref(value))],
      singleField ? None : Some(mkref(record)),
    ),
  );

let findAttr = find_attr;

let hasAttr = (s, attrs) => findAttr(s, attrs) != None;

let refractiveAnnotated = ty =>
  hasAttr("react.loadable", ty.pvb_attributes);

let anyRefractiveAnnotation = List.exists(refractiveAnnotated);

let mapper = (_, _) =>
        {
          ...default_mapper,
          structure: (mapper, items) => {
            switch (items) {
              | [
                  {pstr_desc: Pstr_value(_recFlag, value_bindings), pstr_loc} as item,
                  ...rest,
                ] when anyRefractiveAnnotation(value_bindings) =>
                let derived =
                  Ast_helper.with_default_loc(pstr_loc, () => [item]
                  );
                derived @ mapper.structure(mapper, rest);
              | [item, ...rest] =>
                let derived = mapper.structure_item(mapper, item);
                [derived, ...mapper.structure(mapper, rest)];
              | [] => []
              };
          },
          expr: (mapper, e) => {
            switch (e.pexp_desc) {
            | Pexp_let(Nonrecursive, _, _) => 
                %expr
                42
            /* If the expression is [%gimme] */
            | Pexp_extension(({txt: "some_text", _}, _payload)) =>
              /* Then replace by 42 */
              %expr
              "expression"
            | _ => default_mapper.expr(mapper, e)
            };
          },
          
        };

let () =
  Migrate_parsetree.(
    Driver.register(~name="form_ppx", Versions.ocaml_409, mapper)
  );
