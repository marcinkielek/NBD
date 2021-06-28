printjson(db.people.find({$and:[{"weight":{$gt: "68"}},{"weight":{$lte: "71.5"}}]}).toArray())
