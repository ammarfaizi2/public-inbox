// thread field processor from notmuch - Copyright 2018 David Bremner
// License: GPL-3.0+ <https://www.gnu.org/licenses/gpl-3.0.txt>
// Disclaimer: Eric doesn't know C++

class ThreadFieldProcessor : public Xapian::FieldProcessor {
public:
	Xapian::Query operator()(const std::string &str);
};

static enum exc_iter xpand_col_iter(Xapian::Query *xqry,
					Xapian::MSetIterator *i,
					unsigned column)
{
	try {
		Xapian::Document doc = i->get_document();
		std::string val = doc.get_value(column);
		// n.b. Xapian 1.4.10+ optimizes `|=' to reduce allocation.
		// operator overloading is confusing, yes :<
		*xqry |= Xapian::Query(Xapian::Query::OP_VALUE_RANGE,
					column, val, val);
	} catch (const Xapian::DatabaseModifiedError &e) {
		AUTO_UNLOCK struct open_locks *lk = lock_shared_maybe(cur_req);
		cur_req->srch->db->reopen();
		NOT_UNUSED(lk);
		return ITER_RETRY;
	} catch (const Xapian::DocNotFoundError &e) { // oh well...
		warnx("doc not found: %s", e.get_description().c_str());
	}
	return ITER_OK;
}

static Xapian::Query qry_xpand_col(Xapian::Query qry, unsigned column)
{
	Xapian::Query xqry = Xapian::Query::MatchNothing;
	Xapian::Enquire enq(*cur_req->srch->db);

	enq.set_weighting_scheme(Xapian::BoolWeight());
	enq.set_query(qry);
	enq.set_collapse_key(column);

	Xapian::MSet mset = enq.get_mset(0, cur_req->srch->db->get_doccount());

	for (Xapian::MSetIterator i = mset.begin(); i != mset.end(); i++)  {
		for (int t = 10; t > 0; --t)
			switch (xpand_col_iter(&xqry, &i, column)) {
			case ITER_OK: t = 0; break; // leave inner loop
			case ITER_RETRY: break; // continue for-loop
			case ITER_ABORT: return xqry; // impossible
			}
	}
	return xqry;
}

// Xapian calls this when processing queries since it's registered by
// ->add_boolean_prefix
Xapian::Query ThreadFieldProcessor::operator()(const std::string &str)
{
	Xapian::Query qry;

	if (str.at(0) != '{') { // thread:$MSGID (no `{'/`}' encasement)
		qry = Xapian::Query(Xapian::Query::OP_OR,
				Xapian::Query("Q" + str),
				Xapian::Query("XRF" + str));
	} else if (str.size() <= 1 || str.at(str.size() - 1) != '}') {
		throw Xapian::QueryParserError("missing } in '" + str + "'");
	} else { // thread:"{hello world}"
		std::string qstr = str.substr(1, str.size() - 2);
		qry = cur_req->srch->qp->parse_query(qstr,
						cur_req->srch->qp_flags);
	}
	return qry_xpand_col(qry, THREADID);
}
