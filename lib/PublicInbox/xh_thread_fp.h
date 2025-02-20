// thread field processor from notmuch - Copyright 2018 David Bremner
// License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
// Disclaimer: Eric doesn't know C++

class ThreadFieldProcessor : public Xapian::FieldProcessor {
protected:
	Xapian::QueryParser &qp;
public:
	ThreadFieldProcessor(Xapian::QueryParser &qp_) : qp(qp_) {};
	Xapian::Query operator()(const std::string &str);
};

static enum exc_iter xpand_col_iter(std::set<std::string> &vals,
					Xapian::MSetIterator *i,
					unsigned column)
{
	try {
		Xapian::Document doc = i->get_document();
		vals.insert(doc.get_value(column));
	} catch (const Xapian::DatabaseModifiedError &e) {
		cur_srch->db->reopen();
		return ITER_RETRY;
	} catch (const Xapian::DocNotFoundError &e) { // oh well...
		warnx("doc not found: %s", e.get_description().c_str());
	}
	return ITER_OK;
}

static Xapian::Query qry_xpand_col(Xapian::Query qry, unsigned column)
{
	Xapian::Query xqry = Xapian::Query::MatchNothing;

	Xapian::Enquire enq(*cur_srch->db);
	std::set<std::string> vals; // serialised Xapian column

	enq.set_weighting_scheme(Xapian::BoolWeight());
	enq.set_query(qry);
	enq.set_collapse_key(column);

	Xapian::MSet mset = enq.get_mset(0, cur_srch->db->get_doccount());

	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++)  {
		for (int t = 10; t > 0; --t)
			switch (xpand_col_iter(vals, &i, column)) {
			case ITER_OK: t = 0; break; // leave inner loop
			case ITER_RETRY: break; // continue for-loop
			case ITER_ABORT: return xqry; // impossible
			}
	}

	std::set<std::string>::const_iterator tid;
	for (tid = vals.begin(); tid != vals.end(); tid++)
		xqry = Xapian::Query(Xapian::Query::OP_OR, xqry,
				Xapian::Query(
					Xapian::Query::OP_VALUE_RANGE,
					column, *tid, *tid));
	return xqry;
}

// Xapian calls this when processing queries since it's registered by
// ->add_boolean_prefix
Xapian::Query ThreadFieldProcessor::operator()(const std::string &str)
{
	Xapian::Query qry;

	if (str.at(0) != '{') { // thread:$MSGID (no `{'/`}' encasement)
		qry = Xapian::Query("Q" + str);
	} else if (str.size() <= 1 || str.at(str.size() - 1) != '}') {
		throw Xapian::QueryParserError("missing } in '" + str + "'");
	} else { // thread:"{hello world}"
		std::string qstr = str.substr(1, str.size() - 2);
		qry = cur_srch->qp->parse_query(qstr, cur_srch->qp_flags);
	}
	return qry_xpand_col(qry, THREADID);
}
