/* RTC:
 *
 * Copyright (C) 2011
 *     Geoffrey Biggs
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 * Licensed under the Eclipse Public License -v 1.0 (EPL)
 * http://www.opensource.org/licenses/eclipse-1.0.txt
 *
 * Service implementation class.
 */

#if !defined(SERVICE_IMPL_)
#define SERVICE_IMPL_

#include "idl/service.hh"

namespace Services
{

class <INTERFACE>Provider
    : public virtual POA_RTC::Service,
    public virtual PortableServer::RefCountServantBase
{
    public:
        <INTERFACE>Provider();
        virtual ~<INTERFACE>Provider();

        RTC::<RETURN_TYPE> <METHOD>() throw(CORBA::SystemException);

    private:
}; // class <INTERFACE>Provider

}; // namespace Services

#endif // !defined(SERVICE_IMPL_)

