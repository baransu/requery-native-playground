open TestFramework;
open Library;

let connection_url = "postgresql://postgres:postgres@localhost:5432/db_name";

let pool = Ezpostgresql.Pool.create(~conninfo=connection_url, ~size=8, ());

let create_table = () => {
  let table_name = QueryBuilder.tname("authors");

  QueryBuilder.(
    [
      cdef("id", Types.text, ~primaryKey=true),
      cdef("first", Types.text),
      cdef("last", Types.text),
      constraint_(unique([cname("first"), cname("last")])),
    ]
    |> createTable(table_name, ~ifNotExists=true)
  );
};

let select_table = () => {
  let table_name = QueryBuilder.tname("authors");

  QueryBuilder.(
    select(
      [e(col("id")), e(col("first")), e(col("last"))]
      |> from(table(table_name)),
    )
  );
};

let insert_table = () => {
  let table_name = QueryBuilder.tname("authors");

  RowEncode.(
    [
      ("1", "Stephen", "King"),
      ("2", "Jane", "Austen"),
      ("3", "Kurt", "Vonnegut"),
    ]
    |> insertMany(columns3("id", string, "first", string, "last", string))
    |> into(table_name)
  );
};

type user = {
  id: string,
  first: string,
  last: string,
};

// TODO: fromStringRow
// TODO: toStringRow

let alternative_insert_table = () => {
  let table_name = QueryBuilder.tname("authors");

  RowEncode.(
    [{id: "4", first: "Stephen", last: "King"}]
    |> insertMany(user =>
         stringRow([
           ("id", string(user.id)),
           ("first", string(user.first)),
           ("last", string(user.last)),
         ])
       )
    |> into(table_name)
  );
};

describe("QueryBuilder", ({test, _}) => {
  test("CREATE TABLE", ({expect}) => {
    let create_table_query =
      Sql.CreateTable(create_table()) |> Postgres.render;
    expect.string(create_table_query).toMatchSnapshot();
  });

  test("SELECT", ({expect}) => {
    let select_table_query = Sql.Select(select_table()) |> Postgres.render;
    expect.string(select_table_query).toMatchSnapshot();
  });

  test("creates table, inserts rows and selectes them", ({expect}) => {
    let create_table_query =
      Sql.CreateTable(create_table()) |> Postgres.render;
    let Ok(_) =
      pool
      |> Ezpostgresql.Pool.command(~query=create_table_query)
      |> Lwt_main.run;

    let insert_table_query = Sql.Insert(insert_table()) |> Postgres.render;
    let Ok(_) =
      pool
      |> Ezpostgresql.Pool.command(~query=insert_table_query)
      |> Lwt_main.run;

    let select_table_query = Sql.Select(select_table()) |> Postgres.render;
    let Ok(result) =
      pool |> Ezpostgresql.Pool.all(~query=select_table_query) |> Lwt_main.run;

    Console.log(result);

    expect.int(result |> Array.length).toBe(3);
  });
  // TODO: INSERT returning
});
