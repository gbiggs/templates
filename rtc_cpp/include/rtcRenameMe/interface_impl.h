/* RTC<component>
 *
 * Header file for the ??? interface implementation.
 *
 * Copyright 2010-2011 Geoffrey Biggs geoffrey.biggs@aist.go.jp
 *     RT-Synthesis Research Group
 *     Intelligent Systems Research Institute,
 *     National Institute of Advanced Industrial Science and Technology (AIST),
 *     Japan
 *     All rights reserved.
 *
 * This file is part of RTC<component>.
 *
 * RTC<component> is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *
 * RTC<component> is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with RTC<component>. If not, see <http://www.gnu.org/licenses/>.
 */

#if !defined(INTERFACE_IMPL_H_)
#define INTERFACE_IMPL_H_

#include "idl/interface.hh"

class RTC<component>;

namespace <module>
{

class <interface>Provider
    : public virtual POA_<module>::<interface>,
    public virtual PortableServer::RefCountServantBase
{
    public:
        <interface>Provider();
        virtual ~<interface>Provider();

        void setup(RTC<component>* owner) { owner_ = owner; }

    private:
        RTC<component>* owner_;
}; // class <interface>Provider

}; // namespace RTCHokuyoAist

#endif // INTERFACE_IMPL_H_

