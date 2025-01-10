from genson import SchemaBuilder


builder = SchemaBuilder()
builder.add_object({
    "name": "Alice",
    "age": 30
})
schema = builder.to_schema()

print(schema)

builder2 = SchemaBuilder()

builder2.add_object(
    [{"eruptions":3.6,"waiting":79},{"eruptions":1.8,"waiting":54},{"eruptions":3.333,"waiting":74},{"eruptions":2.283,"waiting":62},{"eruptions":4.533,"waiting":85},{"eruptions":2.883,"waiting":55},{"eruptions":4.7,"waiting":88},{"eruptions":3.6,"waiting":85},{"eruptions":1.95,"waiting":51},{"eruptions":4.35,"waiting":85},{"eruptions":1.833,"waiting":54},{"eruptions":3.917,"waiting":84},{"eruptions":4.2,"waiting":78},{"eruptions":1.75,"waiting":47},{"eruptions":4.7,"waiting":83},{"eruptions":2.167,"waiting":52},{"eruptions":1.75,"waiting":62},{"eruptions":4.8,"waiting":84},{"eruptions":1.6,"waiting":52},{"eruptions":4.25,"waiting":79},{"eruptions":1.8,"waiting":51},{"eruptions":1.75,"waiting":47},{"eruptions":3.45,"waiting":78},{"eruptions":3.067,"waiting":69},{"eruptions":4.533,"waiting":74},{"eruptions":3.6,"waiting":83},{"eruptions":1.967,"waiting":55},{"eruptions":4.083,"waiting":76},{"eruptions":3.85,"waiting":78},{"eruptions":4.433,"waiting":79}]
    )

schema = builder2.to_schema()
print(schema)
